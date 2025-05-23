# Usage Guide: `beautify_active_matlab_script.m`

## 1. Overview

`beautify_active_matlab_script.m` is a convenient wrapper function that allows you to format the MATLAB script or function file currently open and active in your MATLAB editor. It uses the main `code_beautifier.m` function internally to perform the code formatting, applying its various options and style presets.

This provides a quick way to beautify your code without needing to manually pass file paths or code strings to `code_beautifier.m`.

## 2. Prerequisites

*   **MATLAB Version**: Requires a MATLAB version that supports the `matlab.desktop.editor` API. This is generally available in MATLAB R2011b and later, with more robust support and features (like handling unsaved "Untitled" files) in versions like R2019b and newer.
*   **MATLAB Path**: Both `code_beautifier.m` and `beautify_active_matlab_script.m` files must be located on the MATLAB path or in the current MATLAB working directory.

## 3. Basic Usage

To beautify the currently active script in the MATLAB editor, simply run the following command in the MATLAB Command Window:

```matlab
beautify_active_matlab_script
```

This will apply the default beautification settings defined in `code_beautifier.m` (which typically correspond to the `'Default'` style preset unless a `.mbeautifyrc` configuration file is present and specifies otherwise).

## 4. Passing Options

You can customize the beautification process by passing any Name-Value pair arguments accepted by `code_beautifier.m` directly to `beautify_active_matlab_script.m`.

Here are some examples:

*   **Use a specific style preset**:
    ```matlab
    beautify_active_matlab_script('StylePreset', 'CompactStyle')
    ```

*   **Enable `arguments` block formatting and set indent size**:
    ```matlab
    beautify_active_matlab_script('FormatArgumentsBlock', true, 'IndentSize', 2)
    ```

*   **Align assignment statements**:
    ```matlab
    beautify_active_matlab_script('AlignAssignments', true)
    ```

*   **Use settings from a `.mbeautifyrc` file**:
    If you have a `.mbeautifyrc` file in the current working directory (or in a directory that `code_beautifier.m` is configured to check), its settings will be automatically loaded by `code_beautifier.m`. You can simply call:
    ```matlab
    beautify_active_matlab_script
    ```
    For instance, if your `.mbeautifyrc` file contains `FormatArgumentsBlock = true`, this setting will be applied when you run the command. Direct arguments will still override `.mbeautifyrc` settings.

## 5. Convenient Ways to Use (Suggestions)

Typing the command repeatedly can be cumbersome. Here are some ways to make it more accessible:

*   **MATLAB Shortcuts**:
    You can create a keyboard shortcut or a button in the MATLAB Editor or Quick Access Toolbar.
    1.  Navigate to "Preferences" (or "Settings") -> "Keyboard" -> "Shortcuts".
    2.  Alternatively, for the Quick Access Toolbar, right-click the toolbar (e.g., next to the "Save" icon) and select "Customize...".
    3.  Look for options to create a new shortcut.
    4.  Set the "Code" or "Command" for the shortcut to:
        ```matlab
        beautify_active_matlab_script
        ```
    5.  You can then assign a key combination (e.g., `Ctrl+Shift+B`) or add a button to the toolbar. Now, pressing the keys or clicking the button will beautify the active script.
    6.  To pass specific options, include them in the command:
        ```matlab
        beautify_active_matlab_script('FormatArgumentsBlock', true)
        ```

*   **Custom Menu Item (Advanced)**:
    For users comfortable with advanced MATLAB customizations, it might be possible to add a custom item to the MATLAB editor's context menu. This typically involves editing internal MATLAB configuration files (like `MATLABInternalNavigator.xml` or similar, depending on the MATLAB version).
    **Caution**: This method is advanced, not officially supported for this purpose, and carries risks. Incorrect modifications can lead to MATLAB instability. It's generally recommended to use shortcuts.

## 6. Behavior Notes

*   **Direct Editor Update**: The content of the active script in the MATLAB editor is updated directly with the beautified code.
*   **Manual Save Required**: By default, `beautify_active_matlab_script.m` **does not automatically save** the file to disk after beautification. You will need to manually save the script (e.g., `Ctrl+S` or File -> Save) to persist the changes. This is the default behavior to prevent accidental overwrites. Future enhancements to the script could potentially include an auto-save option.
*   **Unsaved ("Untitled") Scripts**: The function can beautify unsaved "Untitled" scripts based on their current content in the editor. You will still need to save them manually if you wish to keep the changes.
*   **Command Window Feedback**: The function provides feedback in the MATLAB Command Window, indicating which script is being beautified, if changes were made, or if any errors occurred.

## 7. Error Handling

*   **API Availability**: If your MATLAB version does not support the necessary `matlab.desktop.editor` API, an error dialog will be displayed.
*   **Beautification Errors**: Any errors encountered during the beautification process (e.g., syntax errors in the code being beautified that `code_beautifier.m` cannot handle, or issues within `code_beautifier.m` itself) will be reported, typically with an error message in the Command Window and potentially an error dialog.
*   **No Active Script**: If no script is currently active in the editor, a message will be displayed in the Command Window.
*   **Non-`.m` Files**: If the active document is not a MATLAB code file (e.g., a `.txt` or `.dat` file), a message will be displayed, and no action will be taken.

---
This guide should help you effectively use `beautify_active_matlab_script.m` to enhance your MATLAB coding workflow.
