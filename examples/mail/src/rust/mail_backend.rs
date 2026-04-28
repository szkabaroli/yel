//! Mail backend — business logic for the `MailPanel` Yel component.
//!
//! This module is compiled as a WebAssembly component that implements the
//! `MailBackend` interface declared in `mail_panel.yel`. It owns *all*
//! non-UI state: JMAP session, message cache, selection set, filtering,
//! sorting, threading, formatting, and smart-card parsing.
//!
//! Yel never sees a Set, a Date, an Intl formatter, or an HTTP request —
//! every surface-facing value is a primitive or a record defined in
//! `types.yel`, and every mutation goes through a `func` entry point that
//! updates a signal the UI is subscribed to.
//!
//! ```text
//! ┌──────────────────┐   signals (records)   ┌───────────────────┐
//! │  Yel MailPanel   │ ◀─────────────────────│  MailStore (Rust) │
//! │ (UI + local fsm) │   commands (funcs)    │  JMAP + cache     │
//! │                  │ ─────────────────────▶│                   │
//! └──────────────────┘                       └───────────────────┘
//! ```

use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::{Arc, Mutex};

// Generated bindings for `meshx:mail@0.1.0`
wit_bindgen::generate!({
    world: "mail-backend",
    path: "../wit",
});

use self::meshx::mail::types::{
    Account, Address, FlatRow, Label, LabelColor, MailboxUnread, Message, MoveTarget,
    SmartCardEntry, SmartCardKind, SmartCardRef, StorageQuota, Thread,
};
use self::meshx::mail::types::{Filter, Mailbox, ReplyMode, Sort};

// ============================================================================
// Store
// ============================================================================

/// Authoritative mail state. Every command mutates this; every signal we push
/// to Yel is derived from it.
pub struct MailStore {
    // ---- Session ----
    session: Option<JmapSession>,
    account: Account,

    // ---- Data ----
    messages: Vec<Message>,
    labels: Vec<Label>,
    mailboxes: Vec<MailboxUnread>,

    // ---- UI-adjacent state (kept in Rust so sorting/derivation is trivial) ----
    current_mailbox: Mailbox,
    current_label: Option<String>,
    search: String,
    filter: Filter,
    sort: Sort,

    selected_ids: BTreeSet<Selection>,
    expanded_threads: HashSet<String>,
    last_selected: Option<Selection>, // for shift-click range
    focused_row: Option<String>,

    reply_mode: ReplyMode,
    compose_open: bool,
    smart_card_index: u32,

    // Smart-card handle table: SmartCardView renders via `handle`.
    smart_cards: HashMap<i32, SmartCardPayload>,
    next_smart_handle: i32,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
enum Selection {
    Message(String),
    Thread(String),
}

struct JmapSession {
    server_url: String,
    bearer: String,
    email: String,
}

/// Full smart-card body. Kept server-side; Yel only sees kind + handle.
#[derive(Clone, Debug)]
pub struct SmartCardPayload {
    pub kind: SmartCardKind,
    pub raw_json: String,
}

// ============================================================================
// Public entry points (generated `Guest` impl bound here)
// ============================================================================

pub struct Component;

impl Guest for Component {
    // ---- Queries ----

    fn format_full_date(epoch_ms: i64) -> String {
        let secs = epoch_ms / 1000;
        let datetime = time::OffsetDateTime::from_unix_timestamp(secs).unwrap_or(time::OffsetDateTime::UNIX_EPOCH);
        format!(
            "{}. {} {}. at {:02}:{:02}",
            datetime.year(),
            month_name(datetime.month()),
            datetime.day(),
            datetime.hour(),
            datetime.minute(),
        )
    }

    fn lookup_label(id: String) -> Option<Label> {
        STORE.lock().unwrap().labels.iter().find(|l| l.id == id).cloned()
    }

    fn label_exists(id: String) -> bool {
        STORE.lock().unwrap().labels.iter().any(|l| l.id == id)
    }

    // ---- Navigation ----

    fn set_mailbox(mailbox: Mailbox) {
        let mut s = STORE.lock().unwrap();
        s.current_mailbox = mailbox;
        s.current_label = None;
        push_all();
    }

    fn set_label_filter(label_id: String) {
        let mut s = STORE.lock().unwrap();
        s.current_label = Some(label_id);
        s.current_mailbox = Mailbox::Inbox;
        push_all();
    }

    fn clear_label_filter() {
        STORE.lock().unwrap().current_label = None;
        push_all();
    }

    fn set_search(q: String) {
        STORE.lock().unwrap().search = q;
        push_rows();
    }

    fn set_filter(f: Filter) {
        STORE.lock().unwrap().filter = f;
        push_rows();
    }

    fn set_sort(s: Sort) {
        STORE.lock().unwrap().sort = s;
        push_rows();
    }

    // ---- Selection ----

    fn select_only(id: String) {
        let mut s = STORE.lock().unwrap();
        s.selected_ids.clear();
        s.selected_ids.insert(Selection::Message(id.clone()));
        s.last_selected = Some(Selection::Message(id));
        s.reply_mode = ReplyMode::None;
        push_selection(&mut s);
    }

    fn select_thread(thread_id: String) {
        let mut s = STORE.lock().unwrap();
        s.selected_ids.clear();
        s.selected_ids.insert(Selection::Thread(thread_id.clone()));
        s.last_selected = Some(Selection::Thread(thread_id));
        s.reply_mode = ReplyMode::None;
        push_selection(&mut s);
    }

    fn toggle_select(id: String) {
        let mut s = STORE.lock().unwrap();
        let sel = Selection::Message(id);
        if !s.selected_ids.remove(&sel) {
            s.selected_ids.insert(sel.clone());
            s.last_selected = Some(sel);
        }
        push_selection(&mut s);
    }

    fn extend_selection_to(id: String) {
        let mut s = STORE.lock().unwrap();
        if let Some(Selection::Message(anchor)) = s.last_selected.clone() {
            let rows = visible_rows(&s);
            let ids: Vec<_> = rows.iter().map(|r| r.msg.id.clone()).collect();
            let a = ids.iter().position(|x| *x == anchor);
            let b = ids.iter().position(|x| *x == id);
            if let (Some(a), Some(b)) = (a, b) {
                let (lo, hi) = if a < b { (a, b) } else { (b, a) };
                for i in lo..=hi {
                    s.selected_ids.insert(Selection::Message(ids[i].clone()));
                }
            }
        }
        push_selection(&mut s);
    }

    fn clear_selection() {
        let mut s = STORE.lock().unwrap();
        s.selected_ids.clear();
        push_selection(&mut s);
    }

    fn toggle_thread_expanded(thread_id: String) {
        let mut s = STORE.lock().unwrap();
        if !s.expanded_threads.remove(&thread_id) {
            s.expanded_threads.insert(thread_id);
        }
        push_rows();
    }

    fn focus_next_row() { /* host-owned focus ring */ }
    fn focus_prev_row() { /* host-owned focus ring */ }

    // ---- Per-message mutations ----

    fn mark_read(id: String) {
        let mut s = STORE.lock().unwrap();
        if let Some(m) = s.messages.iter_mut().find(|m| m.id == id) {
            if !m.read {
                m.read = true;
                if let Some(session) = &s.session {
                    jmap::patch_read(session, &id, true);
                }
            }
        }
        push_counts(&mut s);
        push_rows();
    }

    fn toggle_flag(id: String) {
        let mut s = STORE.lock().unwrap();
        if let Some(m) = s.messages.iter_mut().find(|m| m.id == id) {
            m.flagged = !m.flagged;
            if let Some(session) = &s.session {
                jmap::patch_flagged(session, &id, m.flagged);
            }
        }
        push_rows();
    }

    fn toggle_read(id: String) {
        let mut s = STORE.lock().unwrap();
        if let Some(m) = s.messages.iter_mut().find(|m| m.id == id) {
            m.read = !m.read;
            if let Some(session) = &s.session {
                jmap::patch_read(session, &id, m.read);
            }
        }
        push_counts(&mut s);
        push_rows();
    }

    fn fetch_body(id: String) {
        // Fire-and-forget; signals update once the fetch resolves.
        let session = STORE.lock().unwrap().session.clone();
        if let Some(session) = session {
            jmap::fetch_body_async(session, id, |message| {
                let mut s = STORE.lock().unwrap();
                if let Some(existing) = s.messages.iter_mut().find(|m| m.id == message.id) {
                    existing.body = message.body;
                    existing.body_loading = false;
                }
                push_rows();
            });
        }
    }

    // ---- Bulk mutations ----

    fn bulk_set_read(read: bool) {
        let mut s = STORE.lock().unwrap();
        let ids: Vec<_> = message_ids_in_selection(&s);
        for id in ids {
            if let Some(m) = s.messages.iter_mut().find(|m| m.id == id) {
                m.read = read;
            }
        }
        push_counts(&mut s);
        push_rows();
    }

    fn bulk_toggle_flag() {
        let mut s = STORE.lock().unwrap();
        let ids: Vec<_> = message_ids_in_selection(&s);
        let any_unflagged = ids.iter().any(|id| {
            s.messages.iter().find(|m| &m.id == id).map_or(false, |m| !m.flagged)
        });
        for id in ids {
            if let Some(m) = s.messages.iter_mut().find(|m| m.id == id) {
                m.flagged = any_unflagged;
            }
        }
        push_rows();
    }

    fn bulk_move(mailbox: Mailbox) {
        let mut s = STORE.lock().unwrap();
        let ids: Vec<_> = message_ids_in_selection(&s);
        for id in ids {
            if let Some(m) = s.messages.iter_mut().find(|m| m.id == id) {
                m.mailbox = mailbox;
            }
        }
        s.selected_ids.clear();
        push_counts(&mut s);
        push_rows();
        push_selection(&mut s);
    }

    // ---- Labels ----

    fn update_message_labels(id: String, labels: Vec<String>) {
        let mut s = STORE.lock().unwrap();
        if let Some(m) = s.messages.iter_mut().find(|m| m.id == id) {
            m.labels = labels;
        }
        push_rows();
    }

    fn add_label(name: String, color: LabelColor) -> String {
        let mut s = STORE.lock().unwrap();
        let id = format!("l-{}", slugify(&name));
        s.labels.push(Label { id: id.clone(), name, color });
        push_labels(&s);
        id
    }

    fn create_label_from_prompt() {
        // The host surfaces a native prompt and re-enters via add_label +
        // update_message_labels once it has a name.
        host::prompt_for_label_name();
    }

    // ---- Compose / reply ----

    fn open_compose()  { STORE.lock().unwrap().compose_open = true; }
    fn close_compose() { STORE.lock().unwrap().compose_open = false; }

    fn send_compose(to: String, body: String) {
        let s = STORE.lock().unwrap();
        if let Some(session) = &s.session {
            jmap::send(session, &to, &body);
        }
    }
    fn schedule_compose(_to: String, _body: String) { /* future: scheduled-send */ }

    fn start_reply(mode: ReplyMode)  { STORE.lock().unwrap().reply_mode = mode; }
    fn cancel_reply()                { STORE.lock().unwrap().reply_mode = ReplyMode::None; }

    fn send_reply(body: String, forward_to: String) {
        let mut s = STORE.lock().unwrap();
        let mode = s.reply_mode;
        if let Some(session) = &s.session {
            match mode {
                ReplyMode::Forward   => jmap::forward(session, &forward_to, &body),
                ReplyMode::Reply     => jmap::reply(session, &body, false),
                ReplyMode::ReplyAll  => jmap::reply(session, &body, true),
                ReplyMode::None      => {}
            }
        }
        s.reply_mode = ReplyMode::None;
    }

    // ---- Smart card carousel ----

    fn smart_card_prev() {
        let mut s = STORE.lock().unwrap();
        if s.smart_card_index > 0 { s.smart_card_index -= 1; }
    }
    fn smart_card_next() {
        let mut s = STORE.lock().unwrap();
        s.smart_card_index += 1;
    }

    // ---- Keybinding dispatch ----

    fn handle_action(name: String) {
        let mut s = STORE.lock().unwrap();
        match name.as_str() {
            "DESELECT:meshx://mail" => { s.selected_ids.clear(); push_selection(&mut s); }
            "ARCHIVE:meshx://mail"  => {
                drop(s);
                <Component as Guest>::bulk_move(Mailbox::Archive);
            }
            "DELETE:meshx://mail"   => {
                drop(s);
                <Component as Guest>::bulk_move(Mailbox::Trash);
            }
            _ => {}
        }
    }

    // ---- Session bootstrap ----

    fn boot() {
        let params = host::read_page_params();
        let mut s = STORE.lock().unwrap();
        if let (Some(token), Some(url), Some(email)) = (params.token, params.backend_url, params.email) {
            s.session = Some(JmapSession {
                server_url: url,
                bearer: format!("Bearer {}", token),
                email,
            });
            drop(s);
            host::when_service_ready("meshx.mail.jmap", || {
                let s = STORE.lock().unwrap();
                if let Some(session) = &s.session {
                    jmap::connect(session);
                }
            });
        } else {
            // No session — leave mock data loaded (fixtures).
            push_all();
        }
    }
}

// ============================================================================
// Derivation: computed state pushed to Yel signals
// ============================================================================

fn push_all() {
    let s = STORE.lock().unwrap();
    push_labels(&s);
    drop(s);

    let mut s = STORE.lock().unwrap();
    push_counts(&mut s);
    push_selection(&mut s);
    drop(s);

    push_rows();
    push_storage();
}

fn push_labels(s: &MailStore) {
    signals::set_labels(s.labels.clone());
}

fn push_counts(s: &mut MailStore) {
    let counts = if s.session.is_some() {
        // Server-provided counts pre-populated into mailboxes table
        s.mailboxes.clone()
    } else {
        derive_counts(&s.messages)
    };
    signals::set_mailbox_counts(counts);
}

/// The heart of the Svelte `visibleMessages` → `visibleThreads` → `flatRows`
/// pipeline: produces a single flat list of rows ready for the UI.
fn push_rows() {
    let s = STORE.lock().unwrap();
    let rows = visible_rows(&s);
    signals::set_flat_rows(rows.clone());
    signals::set_visible_message_count(rows.iter().filter(|r| !r.is_thread_header).count() as i32);
    signals::set_page_title(page_title(&s));
}

fn visible_rows(s: &MailStore) -> Vec<FlatRow> {
    // 1. filter by mailbox/label
    let in_box: Vec<&Message> = s
        .messages
        .iter()
        .filter(|m| {
            if let Some(lbl) = &s.current_label {
                m.labels.contains(lbl) && m.mailbox != Mailbox::Trash
            } else if s.current_mailbox == Mailbox::Flagged {
                m.flagged && m.mailbox != Mailbox::Trash
            } else {
                m.mailbox == s.current_mailbox
            }
        })
        .collect();

    // 2. search
    let lowered = s.search.to_lowercase();
    let after_search: Vec<&Message> = if lowered.is_empty() {
        in_box
    } else {
        in_box
            .into_iter()
            .filter(|m| {
                m.subject.to_lowercase().contains(&lowered)
                    || m.from.name.to_lowercase().contains(&lowered)
                    || m.preview.to_lowercase().contains(&lowered)
            })
            .collect()
    };

    // 3. filter
    let me = &s.account;
    let after_filter: Vec<&Message> = after_search
        .into_iter()
        .filter(|m| match s.filter {
            Filter::All         => true,
            Filter::Unread      => !m.read,
            Filter::Flagged     => m.flagged,
            Filter::Attachments => m.has_attachments,
            Filter::MentionsMe  => {
                m.body.to_lowercase().contains(&me.name.to_lowercase())
                    || m.body.to_lowercase().contains(&me.email.to_lowercase())
            }
            Filter::ToMe => m.to.iter().any(|a| a.email == me.email),
        })
        .collect();

    // 4. sort
    let mut after_sort: Vec<Message> = after_filter.into_iter().cloned().collect();
    match s.sort {
        Sort::Date    => after_sort.sort_by(|a, b| b.date_display.cmp(&a.date_display)),
        Sort::Sender  => after_sort.sort_by(|a, b| a.from.name.cmp(&b.from.name)),
        Sort::Subject => after_sort.sort_by(|a, b| a.subject.cmp(&b.subject)),
    }

    // 5. thread grouping
    let mut thread_map: HashMap<String, Vec<Message>> = HashMap::new();
    let mut order: Vec<String> = Vec::new();
    for m in after_sort {
        if !thread_map.contains_key(&m.thread_id) {
            order.push(m.thread_id.clone());
        }
        thread_map.entry(m.thread_id.clone()).or_default().push(m);
    }
    let threads: Vec<Thread> = order
        .into_iter()
        .map(|tid| {
            let mut msgs = thread_map.remove(&tid).unwrap();
            msgs.sort_by(|a, b| b.date_display.cmp(&a.date_display));
            let latest = msgs[0].clone();
            let count = msgs.len() as i32;
            let has_unread = msgs.iter().any(|m| !m.read);
            Thread { thread_id: tid, latest, messages: msgs, count, has_unread }
        })
        .collect();

    // 6. flatten with tint + rounding
    let mut out: Vec<FlatRow> = Vec::new();
    for t in &threads {
        let is_expanded = s.expanded_threads.contains(&t.thread_id) && t.count > 1;
        let is_thread = t.count > 1;

        if is_expanded {
            let header_selected = s.selected_ids.contains(&Selection::Thread(t.thread_id.clone()));
            out.push(FlatRow {
                msg: t.latest.clone(),
                thread_id: t.thread_id.clone(),
                thread_count: t.count,
                is_expanded: true,
                is_first: true,
                is_last: false,
                is_thread_header: true,
                tint: if header_selected { 3 } else { 2 },
                show_divider: false,
                round_top: true,
                round_bottom: false,
                unread_dot: t.has_unread,
            });
            for (i, m) in t.messages.iter().enumerate() {
                let selected = s.selected_ids.contains(&Selection::Message(m.id.clone()));
                out.push(FlatRow {
                    msg: m.clone(),
                    thread_id: t.thread_id.clone(),
                    thread_count: t.count,
                    is_expanded: true,
                    is_first: false,
                    is_last: i == t.messages.len() - 1,
                    is_thread_header: false,
                    tint: if selected { 3 } else { 1 },
                    show_divider: false,
                    round_top: false,
                    round_bottom: i == t.messages.len() - 1,
                    unread_dot: !m.read,
                });
            }
        } else {
            let selected = if is_thread {
                s.selected_ids.contains(&Selection::Thread(t.thread_id.clone()))
            } else {
                s.selected_ids.contains(&Selection::Message(t.latest.id.clone()))
            };
            out.push(FlatRow {
                msg: t.latest.clone(),
                thread_id: t.thread_id.clone(),
                thread_count: t.count,
                is_expanded: false,
                is_first: true,
                is_last: true,
                is_thread_header: is_thread,
                tint: if selected { 3 } else if is_thread { 2 } else { 0 },
                show_divider: false,
                round_top: true,
                round_bottom: true,
                unread_dot: t.has_unread,
            });
        }
    }

    // 7. divider pass (only between tint-0 rows)
    for i in 1..out.len() {
        let prev_tint = out[i - 1].tint;
        let cur_tint = out[i].tint;
        if prev_tint == 0 && cur_tint == 0 {
            out[i].show_divider = true;
        }
    }

    out
}

fn push_selection(s: &mut MailStore) {
    signals::set_selection_count(s.selected_ids.len() as i32);

    // Preview stack: last 5
    let preview: Vec<Message> = s
        .selected_ids
        .iter()
        .rev()
        .take(5)
        .filter_map(|sel| match sel {
            Selection::Message(id) => s.messages.iter().find(|m| &m.id == id).cloned(),
            Selection::Thread(_) => None,
        })
        .collect();
    signals::set_selection_preview(preview);

    // Single-selection detail state
    let (selected_message, selected_thread, show_thread_view) = if s.selected_ids.len() == 1 {
        let sel = s.selected_ids.iter().next().unwrap().clone();
        match sel {
            Selection::Message(id) => {
                let msg = s.messages.iter().find(|m| m.id == id).cloned();
                let thread = msg.as_ref().and_then(|m| build_thread(&s.messages, &m.thread_id));
                (msg, thread, false)
            }
            Selection::Thread(tid) => {
                let thread = build_thread(&s.messages, &tid);
                let latest = thread.as_ref().map(|t| t.latest.clone());
                let show = thread.as_ref().map_or(false, |t| t.count > 1);
                (latest, thread, show)
            }
        }
    } else {
        (None, None, false)
    };

    signals::set_selected_message(selected_message.clone());
    signals::set_selected_thread(selected_thread.clone());
    signals::set_show_thread_view(show_thread_view);

    // Smart card carousel contents
    let entries = selected_thread
        .as_ref()
        .map(|t| {
            if show_thread_view {
                t.messages
                    .iter()
                    .filter(|m| m.smart_card.kind != SmartCardKind::None)
                    .map(|m| SmartCardEntry { card: m.smart_card.clone(), msg_id: m.id.clone() })
                    .collect::<Vec<_>>()
            } else if let Some(m) = &selected_message {
                if m.smart_card.kind != SmartCardKind::None {
                    vec![SmartCardEntry { card: m.smart_card.clone(), msg_id: m.id.clone() }]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            }
        })
        .unwrap_or_default();
    signals::set_thread_smart_cards(entries);

    if s.smart_card_index > 0 {
        // Reset on thread change, mirroring the Svelte $effect
        s.smart_card_index = 0;
    }
}

fn push_storage() {
    let s = STORE.lock().unwrap();
    const TOTAL: i64 = 104_857_600;
    let used: i64 = s
        .messages
        .iter()
        .filter(|m| m.mailbox != Mailbox::Trash)
        .map(|m| 42_000 + if m.has_attachments { 1_200_000 } else { 0 })
        .sum();
    let percent = ((used as f64 / TOTAL as f64) * 100.0).round() as i32;

    signals::set_storage(StorageQuota {
        used,
        total: TOTAL,
        percent,
        used_display: format_bytes(used),
        total_display: format_bytes(TOTAL),
    });
}

// ============================================================================
// Small helpers
// ============================================================================

fn page_title(s: &MailStore) -> String {
    if let Some(id) = &s.current_label {
        return s
            .labels
            .iter()
            .find(|l| &l.id == id)
            .map(|l| l.name.clone())
            .unwrap_or_else(|| "Mail".to_string());
    }
    match s.current_mailbox {
        Mailbox::Inbox   => "Inbox",
        Mailbox::Sent    => "Sent",
        Mailbox::Drafts  => "Drafts",
        Mailbox::Archive => "Archive",
        Mailbox::Spam    => "Spam",
        Mailbox::Trash   => "Trash",
        Mailbox::Flagged => "Flagged",
    }
    .to_string()
}

fn derive_counts(messages: &[Message]) -> Vec<MailboxUnread> {
    use Mailbox::*;
    let count = |mbx: Mailbox| -> i32 {
        messages
            .iter()
            .filter(|m| match mbx {
                Inbox   => m.mailbox == Inbox && !m.read,
                Drafts  => m.mailbox == Drafts,
                Spam    => m.mailbox == Spam,
                Trash   => m.mailbox == Trash,
                Flagged => m.flagged && m.mailbox != Trash,
                _       => false,
            })
            .count() as i32
    };
    vec![
        MailboxUnread { mailbox: Inbox,   unread: count(Inbox)   },
        MailboxUnread { mailbox: Drafts,  unread: count(Drafts)  },
        MailboxUnread { mailbox: Spam,    unread: count(Spam)    },
        MailboxUnread { mailbox: Trash,   unread: count(Trash)   },
        MailboxUnread { mailbox: Flagged, unread: count(Flagged) },
        MailboxUnread { mailbox: Sent,    unread: 0              },
        MailboxUnread { mailbox: Archive, unread: 0              },
    ]
}

fn build_thread(messages: &[Message], tid: &str) -> Option<Thread> {
    let mut msgs: Vec<Message> = messages.iter().filter(|m| m.thread_id == tid).cloned().collect();
    if msgs.is_empty() {
        return None;
    }
    msgs.sort_by(|a, b| b.date_display.cmp(&a.date_display));
    let latest = msgs[0].clone();
    let count = msgs.len() as i32;
    let has_unread = msgs.iter().any(|m| !m.read);
    Some(Thread { thread_id: tid.to_string(), latest, messages: msgs, count, has_unread })
}

fn message_ids_in_selection(s: &MailStore) -> Vec<String> {
    s.selected_ids
        .iter()
        .flat_map(|sel| match sel {
            Selection::Message(id) => vec![id.clone()],
            Selection::Thread(tid) => s
                .messages
                .iter()
                .filter(|m| m.thread_id == *tid)
                .map(|m| m.id.clone())
                .collect(),
        })
        .collect()
}

fn format_bytes(bytes: i64) -> String {
    if bytes == 0                          { return "—".into(); }
    if bytes < 1024                        { return format!("{} B", bytes); }
    if bytes < 1_048_576                   { return format!("{:.1} KB", bytes as f64 / 1024.0); }
    if bytes < 1_073_741_824               { return format!("{:.1} MB", bytes as f64 / 1_048_576.0); }
    format!("{:.1} GB", bytes as f64 / 1_073_741_824.0)
}

fn slugify(name: &str) -> String {
    name.to_lowercase().replace(char::is_whitespace, "-")
}

fn month_name(m: time::Month) -> &'static str {
    use time::Month::*;
    match m {
        January => "January", February => "February", March => "March",
        April   => "April",   May      => "May",      June  => "June",
        July    => "July",    August   => "August",   September => "September",
        October => "October", November => "November", December  => "December",
    }
}

// ============================================================================
// Global store + generated signal setters (push to Yel)
// ============================================================================

lazy_static::lazy_static! {
    static ref STORE: Arc<Mutex<MailStore>> = Arc::new(Mutex::new(MailStore::seed()));
}

impl MailStore {
    fn seed() -> Self {
        // In production, the JMAP `boot()` call replaces everything. For
        // offline dev, load fixtures.
        Self {
            session: None,
            account: fixtures::me(),
            messages: fixtures::mock_messages(),
            labels: fixtures::mock_labels(),
            mailboxes: Vec::new(),
            current_mailbox: Mailbox::Inbox,
            current_label: None,
            search: String::new(),
            filter: Filter::All,
            sort: Sort::Date,
            selected_ids: BTreeSet::new(),
            expanded_threads: HashSet::new(),
            last_selected: None,
            focused_row: None,
            reply_mode: ReplyMode::None,
            compose_open: false,
            smart_card_index: 0,
            smart_cards: HashMap::new(),
            next_smart_handle: 1,
        }
    }
}

// ----------------------------------------------------------------------------
// Thin wrappers around host/jmap/signal FFI. Keeping them isolated here so the
// store logic above is easy to unit-test with fakes.
// ----------------------------------------------------------------------------

mod host {
    pub struct PageParams {
        pub token:       Option<String>,
        pub backend_url: Option<String>,
        pub email:       Option<String>,
    }
    extern "C" {
        pub fn read_page_params() -> PageParams;
        pub fn when_service_ready(name: &str, cb: fn());
        pub fn prompt_for_label_name();
    }
}

mod jmap {
    use super::{JmapSession, Message};
    pub fn connect(_s: &JmapSession)                               {}
    pub fn patch_read(_s: &JmapSession, _id: &str, _read: bool)    {}
    pub fn patch_flagged(_s: &JmapSession, _id: &str, _on: bool)   {}
    pub fn fetch_body_async(_s: JmapSession, _id: String, _cb: fn(Message)) {}
    pub fn send(_s: &JmapSession, _to: &str, _body: &str)          {}
    pub fn reply(_s: &JmapSession, _body: &str, _all: bool)        {}
    pub fn forward(_s: &JmapSession, _to: &str, _body: &str)       {}
}

mod signals {
    use super::*;
    pub fn set_labels(_v: Vec<Label>)                 {}
    pub fn set_mailbox_counts(_v: Vec<MailboxUnread>) {}
    pub fn set_flat_rows(_v: Vec<FlatRow>)            {}
    pub fn set_visible_message_count(_n: i32)         {}
    pub fn set_page_title(_t: String)                 {}
    pub fn set_selection_count(_n: i32)               {}
    pub fn set_selection_preview(_v: Vec<Message>)    {}
    pub fn set_selected_message(_m: Option<Message>)  {}
    pub fn set_selected_thread(_t: Option<Thread>)    {}
    pub fn set_show_thread_view(_b: bool)             {}
    pub fn set_thread_smart_cards(_v: Vec<SmartCardEntry>) {}
    pub fn set_storage(_q: StorageQuota)              {}
}

mod fixtures {
    use super::*;
    pub fn me() -> Account {
        Account { email: "you@example.com".into(), name: "You".into(), avatar: "".into() }
    }
    pub fn mock_messages() -> Vec<Message>   { Vec::new() }
    pub fn mock_labels()   -> Vec<Label>     { Vec::new() }
}

export!(Component);
