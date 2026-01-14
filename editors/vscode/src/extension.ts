import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import { ExtensionContext, window, workspace, commands } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    Executable,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let context: ExtensionContext | undefined;

export function activate(ctx: ExtensionContext) {
    context = ctx;

    // Register restart command
    const restartCommand = commands.registerCommand('yel.restartServer', async () => {
        await restartServer();
    });
    ctx.subscriptions.push(restartCommand);

    // Start the server
    startServer();

    ctx.subscriptions.push({
        dispose: () => {
            if (client) {
                client.stop();
            }
        }
    });
}

async function startServer() {
    if (!context) {
        return;
    }

    const serverPath = getServerPath(context);

    if (!serverPath) {
        window.showErrorMessage(
            'Yel language server not found. Please set yel.server.path in settings or install the bundled binary.'
        );
        return;
    }

    if (!fs.existsSync(serverPath)) {
        window.showErrorMessage(
            `Yel language server not found at: ${serverPath}`
        );
        return;
    }

    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            options: { env: { ...process.env, RUST_LOG: 'info' } }
        } as Executable,
        debug: {
            command: serverPath,
            options: { env: { ...process.env, RUST_LOG: 'debug' } }
        } as Executable,
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'yel' }],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/*.yel'),
        },
        outputChannelName: 'Yel Language Server',
    };

    client = new LanguageClient(
        'yel-lsp',
        'Yel Language Server',
        serverOptions,
        clientOptions
    );

    await client.start();
    window.showInformationMessage('Yel Language Server started');
}

async function restartServer() {
    window.showInformationMessage('Restarting Yel Language Server...');

    if (client) {
        await client.stop();
        client = undefined;
    }

    await startServer();
}

export function deactivate(): Thenable<void> | undefined {
    return client?.stop();
}

function getServerPath(context: ExtensionContext): string | undefined {
    // Check user configuration first
    const config = workspace.getConfiguration('yel');
    const userPath = config.get<string>('server.path');
    if (userPath && fs.existsSync(userPath)) {
        return userPath;
    }

    // Use bundled binary based on platform
    const platform = os.platform();
    const arch = os.arch();

    let binaryName: string;
    switch (platform) {
        case 'darwin':
            binaryName = arch === 'arm64'
                ? 'yel-lsp-darwin-arm64'
                : 'yel-lsp-darwin-x64';
            break;
        case 'linux':
            binaryName = arch === 'arm64'
                ? 'yel-lsp-linux-arm64'
                : 'yel-lsp-linux-x64';
            break;
        case 'win32':
            binaryName = 'yel-lsp-win32-x64.exe';
            break;
        default:
            window.showErrorMessage(`Unsupported platform: ${platform}`);
            return undefined;
    }

    const bundledPath = path.join(context.extensionPath, 'bin', binaryName);
    if (fs.existsSync(bundledPath)) {
        return bundledPath;
    }

    // Fallback: simple binary name (for development)
    const simplePath = path.join(context.extensionPath, 'bin', platform === 'win32' ? 'yel-lsp.exe' : 'yel-lsp');
    if (fs.existsSync(simplePath)) {
        return simplePath;
    }

    // Fallback: check if yel-lsp is in PATH
    const envPath = process.env.PATH || '';
    const pathDirs = envPath.split(path.delimiter);
    for (const dir of pathDirs) {
        const candidate = path.join(dir, platform === 'win32' ? 'yel-lsp.exe' : 'yel-lsp');
        if (fs.existsSync(candidate)) {
            return candidate;
        }
    }

    // Development fallback: look for cargo build output
    const workspaceRoot = path.join(context.extensionPath, '..', '..');
    const devPath = path.join(
        workspaceRoot,
        'target',
        'release',
        platform === 'win32' ? 'yel-lsp.exe' : 'yel-lsp'
    );
    if (fs.existsSync(devPath)) {
        return devPath;
    }

    const debugPath = path.join(
        workspaceRoot,
        'target',
        'debug',
        platform === 'win32' ? 'yel-lsp.exe' : 'yel-lsp'
    );
    if (fs.existsSync(debugPath)) {
        return debugPath;
    }

    return undefined;
}
