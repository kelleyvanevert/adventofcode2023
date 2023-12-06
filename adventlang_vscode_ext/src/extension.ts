import * as vscode from "vscode";

export function activate(context: vscode.ExtensionContext) {
  console.log('Congratulations, your extension "adventlang" is now active!');

  let disposable = vscode.commands.registerCommand(
    "adventlang.helloWorld",
    () => {
      vscode.window.showInformationMessage("Hello World from Adventlang!");
    }
  );

  context.subscriptions.push(disposable);
}

export function deactivate() {}
