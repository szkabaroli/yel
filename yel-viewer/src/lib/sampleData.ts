export interface CodeExample {
  id: string;
  name: string;
  code: string;
}

export const examples: CodeExample[] = [
  {
    id: "counter",
    name: "Counter",
    code: `package yel:counter@1.0.0;

record Person {
    name: string,
    age: u32,
}

enum status { pending, active, completed }

export component Counter {
    count: s32 = 0;
    label: string = "Count";
    items: list<Person> = [{ name: "Alice", age: 30 }];
    numbers: list<u32> = [1, 2, 3];

    incremented: func();

    VStack {
        Text { "{label}: {count}" }

        HStack {
            Button {
                "-"
                clicked: { count -= 1; }
            }
            Button {
                "+"
                clicked: { count += 1; incremented(); }
            }
        }

        if count > 10 {
            Text { "High count!" }
        } else if count < 0 {
            Text { "Negative!" }
        }

        Text { "Items: {items.len()} {items[0].name}" }

        for item in items key(item.name) {
            Text { "{item.name}" }

            for n in numbers key(n) {
                Text { "{n}" }
            }
        }
    }
}`,
  },
  {
    id: "counter-nested",
    name: "Counter (Nested Lists)",
    code: `package yel:counter@1.0.0;

record Item {
    name: string,
    subitems: list<string>,
}

export component Counter {
    count: s32 = 0;
    label: string = "Count";
    items: list<Item> = [
        { name: "Alice", subitems: ["a1", "a2"] },
        { name: "Bob", subitems: ["b1", "b2", "b3"] },
    ];

    VStack {
        Text { "{label}: {count}" }

        HStack {
            Button {
                "-"
                clicked: { count -= 1; }
            }
            Button {
                "+"
                clicked: { count += 1; }
            }
        }

        if count > 10 {
            Text { "High count!" }
        } else if count < 0 {
            Text { "Negative!" }
        }

        for item in items key(item.name) {
            VStack {
                Text { "{item.name}" }
                for sub in item.subitems key(sub) {
                    Text { "- {sub}" }
                }
            }
        }
    }
}`,
  },
  {
    id: "nested-for",
    name: "Nested For Loops",
    code: `package yel:nested@1.0.0;

record Item {
    name: string,
    count: u32,
}

export component NestedFor {
    items: list<Item> = [
        { name: "Alice", count: 2 },
        { name: "Bob", count: 3 },
    ];
    numbers: list<u32> = [1, 2, 3];

    VStack {
        for item in items key(item.name) {
            VStack {
                Text { "{item.name}" }
                HStack {
                    for n in numbers key(n) {
                        Text { "[{n}]" }
                    }
                }
            }
        }
    }
}`,
  },
  {
    id: "nested-parent",
    name: "Nested Parent Access",
    code: `package yel:nested-parent@1.0.0;

record Item {
    name: string,
    subitems: list<string>,
}

export component NestedParent {
    items: list<Item> = [
        { name: "Alice", subitems: ["a1", "a2"] },
        { name: "Bob", subitems: ["b1", "b2", "b3"] },
    ];

    VStack {
        for item in items key(item.name) {
            VStack {
                Text { "{item.name}" }
                for sub in item.subitems key(sub) {
                    Text { "- {sub}" }
                }
            }
        }
    }
}`,
  },
  {
    id: "checkerboard",
    name: "Checkerboard",
    code: `package yel:counter@1.0.0;

export component Counter {
    rows: list<u32> = [0, 1, 2, 3, 4, 5, 6, 7];
    cols: list<u32> = [0, 1, 2, 3, 4, 5, 6, 7];

    VStack {
        for row in rows key(row) {
            HStack {
                for col in cols key(col) {
                    if (row + col) % 2 == 0 {
                        Box {
                            style: "background-color: white; width: 20px; height: 20px;"
                        }
                    } else {
                        Box {
                            style: "background-color: dimgray; width: 20px; height: 20px;"
                        }
                    }
                }
            }
        }
    }
}`,
  },
  {
    id: "temperature",
    name: "Temperature Converter",
    code: `package yel:temperature@1.0.0;

export component TempConverter {
    celsius: f32 = 0.0;
    fahrenheit: f32 = 32.0;

    HStack {
        Input {
            value: celsius
            set value: {
                fahrenheit = 32.0 + (9.0 / 5.0) * celsius;
            }
        }
        Text { "°C = " }
        Input {
            value: fahrenheit
            set value: {
                celsius = (5.0 / 9.0) * (fahrenheit - 32.0);
            }
        }
        Text { "°F" }
    }
}`,
  },
];

// Default sample code (first example)
export const sampleCode = examples[0].code;
