# `code_beautifier.m` - MATLAB Code Formatter

## Introduction

`code_beautifier.m` is a MATLAB script designed to automatically format and beautify MATLAB `.m` files or code snippets. Its goal is to provide "one-click" beautification using a set of sensible default formatting rules, while also offering a range of options for customization to suit individual preferences or coding standards.

## Features

*   **Configurable Indentation:** Use spaces or tabs, with adjustable indent size.
*   **Operator Spacing:** Automatically add spaces around binary operators (e.g., `a + b` instead of `a+b`).
*   **Comma Spacing:** Ensure consistent spacing after commas.
*   **Line Continuation Indentation:** Control additional indentation for continued lines.
*   **Semicolon Management:**
    *   Remove redundant semicolons (e.g., `a=1;;` becomes `a=1;`).
    *   Optionally add semicolons to statements that would otherwise produce output (experimental).
*   **Blank Line Control:**
    *   Preserve or remove blank lines.
    *   Ensure a minimum number of blank lines before major block keywords.
*   **Comment Handling:** Generally preserves comments and their relative indentation, including robust handling of `%` characters within string literals.
*   **Structural Formatting:** Standardizes indentation for control flow blocks (`if`, `for`, `while`, `switch`, etc.).

## Requirements

*   **MATLAB Version:** Tested on MATLAB R2021a and later. String literals with double quotes (`"`) for comments require R2017a or newer for full comment-parsing fidelity due to the introduction of the `string` data type and its handling of double quotes. Basic functionality should work on older versions, but thorough testing on versions prior to R2017a has not been performed.
*   **Dependencies:** Single-file script with no external toolbox dependencies.

## How to Use

### Basic Usage

```matlab
beautifulCode = code_beautifier(rawCode);

% Example with a file:
% rawText = fileread('myScript.m');
% formattedText = code_beautifier(rawText, 'OutputFormat', 'char');
% disp(formattedText);

% % To save back (BE CAREFUL - BACKUP YOUR ORIGINAL):
% % fidOut = fopen('myScript_beautified.m', 'w');
% % fprintf(fidOut, '%s', formattedText);
% % fclose(fidOut);
```

### Input `rawCode`

The `rawCode` input can be:
*   A **character array** (a single string containing the entire code, possibly with newline characters `\n`).
*   A **cell array of strings**, where each cell contains one line of code.
*   A **string array** (from R2016b+), where each element contains one line of code or the entire code as a scalar string.

### Output `beautifulCode`

The format of `beautifulCode` depends on the `OutputFormat` option:
*   If `'cell'` (default): Output is a cell array of strings, each cell representing a formatted line.
*   If `'char'`: Output is a single character array (string) with lines separated by newline characters (`\n`).

### Options (Name-Value Pairs)

The beautifier's behavior can be customized using the following name-value pair arguments:

*   **`IndentSize`**
    *   Purpose: Specifies the number of spaces to use for one indentation level.
    *   Data Type: `scalar integer`
    *   Default: `4`
    *   Example: `code_beautifier(code, 'IndentSize', 2)`

*   **`UseTabs`**
    *   Purpose: If `true`, tabs are used for indentation instead of spaces. `IndentSize` will then correspond to the number of tab characters per level.
    *   Data Type: `logical`
    *   Default: `false`

*   **`SpaceAroundOperators`**
    *   Purpose: If `true`, adds spaces around binary operators (e.g., `+`, `-`, `*`, `/`, `==`, `&&`).
    *   Data Type: `logical`
    *   Default: `true`

*   **`SpaceAfterComma`**
    *   Purpose: If `true`, adds a space after commas.
    *   Data Type: `logical`
    *   Default: `true`

*   **`ContinuationIndentOffset`**
    *   Purpose: Specifies the number of *additional* indent levels for lines continued with `...`. For example, if `IndentSize` is 4 and `ContinuationIndentOffset` is 1, continued lines will be indented by an extra 4 spaces relative to the primary indent of the statement.
    *   Data Type: `scalar integer`
    *   Default: `1`

*   **`PreserveBlankLines`**
    *   Purpose: If `true`, single blank lines in the input code are preserved, and multiple consecutive blank lines are collapsed into one. If `false`, most blank lines are removed (except potentially those enforced by `MinBlankLinesBeforeBlock`).
    *   Data Type: `logical`
    *   Default: `true`

*   **`MinBlankLinesBeforeBlock`**
    *   Purpose: Ensures a minimum number of blank lines (0, 1, or 2) before major block-starting keywords (e.g., `if`, `for`, `function`). This can help visually separate code blocks. Set to 0 to disable. This option will not add blank lines at the very beginning of the file.
    *   Data Type: `scalar integer` (0, 1, or 2)
    *   Default: `0`

*   **`RemoveRedundantSemicolons`**
    *   Purpose: If `true`, removes extraneous semicolons (e.g., `;;` becomes `;`, `end;` becomes `end` unless the semicolon is syntactically required or conventional, like for anonymous functions ending with `end);`).
    *   Data Type: `logical`
    *   Default: `true`

*   **`AddSemicolonsToStatements`**
    *   Purpose: If `true`, attempts to add semicolons to lines that are likely statements or expressions whose output would normally be displayed in the command window (e.g., function calls like `disp('Hello')`, unassigned calculations like `a+b`).
    *   Data Type: `logical`
    *   Default: `false`
    *   **Note:** This option is experimental and should be used with caution. It alters program behavior by suppressing output. While it tries to be intelligent (e.g., not adding semicolons to single variable names on a line meant for display, or to keyword lines), it's based on heuristics. **Always review changes made when this option is enabled.**

*   **`OutputFormat`**
    *   Purpose: Defines the data type of the returned `beautifulCode`.
    *   Data Type: `char` (options: `'cell'` or `'char'`)
    *   Default: `'cell'`

*   **`AlignAssignments`**
    *   Purpose: If `true`, attempts to align the `=` signs in consecutive assignment statements. Blocks of assignments are determined by lines with the same indentation level that are not interrupted by empty lines, comment-only lines, or other code constructs (like control flow keywords).
    *   Data Type: `logical`
    *   Default: `false`
    *   Example:
        ```matlab
        % Input:
        % a=1;
        % longVarName=2;
        % b = 3;
        %
        % Output with AlignAssignments=true:
        % a           = 1;
        % longVarName = 2;
        % b           = 3;
        ```

*   **`SpaceInsideParentheses`**
    *   Purpose: If `true`, adds a single space immediately after an opening parenthesis `(` and immediately before a closing parenthesis `)` if they enclose non-whitespace content. Does not affect empty parentheses `()` or already spaced `( )`.
    *   Data Type: `logical`
    *   Default: `false`
    *   Example (`SpaceInsideParentheses = true`):
        *   `func(a,b)` becomes `func( a, b )`
        *   `matrix(idx)` becomes `matrix( idx )`
        *   `if (condition)` becomes `if ( condition )`
        *   `myFunc()` remains `myFunc()`

*   **`MaxBlankLinesInCode`**
    *   Purpose: Specifies the maximum number of consecutive blank lines to preserve *within* code blocks (e.g., inside functions or control flow blocks). This option is active when `PreserveBlankLines` is `true`. If `PreserveBlankLines` is `false`, most internal blank lines are removed regardless of this option.
    *   Data Type: `scalar integer` (non-negative)
    *   Default: `1` (Meaning, if `PreserveBlankLines` is true, multiple blank lines will be collapsed to a single blank line by default).
    *   Example (`PreserveBlankLines = true`, `MaxBlankLinesInCode = 2`):
        ```matlab
        % Input:
        % line1;
        %
        %
        %
        % line2;
        %
        % Output:
        % line1;
        %
        %
        % line2;
        ```

## Selective Formatting

You can disable and re-enable formatting for specific sections of your code using special comments:

*   **`% beautify_off`**: Disables formatting for all subsequent lines. The line containing this marker itself will still be formatted according to the rules active when it's encountered.
*   **`% beautify_on`**: Re-enables formatting for all subsequent lines. The line containing this marker itself will also be formatted.

Lines within a `beautify_off` ... `beautify_on` block are preserved exactly as they are in the original code, including their indentation and all spacing. This is useful for protecting manually formatted complex layouts or code that the beautifier might not handle optimally.

The markers are checked against the core comment text (e.g., `% beautify_off % some other text` will still be recognized). The on/off state is global and not nestable; the first `off` disables, and the first `on` (after an `off`) re-enables.

### Example of Selective Formatting:
```matlab
% Input:
% formatted_code1 = 1+1; % beautify_on (has no effect if already on)
% % beautify_off
%   manuallyFormatted = [ 1,  2,  3; % This block will be preserved
%                         4,  5,  6];
% % beautify_on
% formatted_code2 = 2+2;

% Output (assuming default options otherwise):
% formatted_code1 = 1 + 1; % beautify_on (has no effect if already on)
% % beautify_off
%   manuallyFormatted = [ 1,  2,  3; % This block will be preserved
%                         4,  5,  6];
% % beautify_on
% formatted_code2 = 2 + 2;
```


## Configuration File (`.mbeautifyrc`)

The beautifier can be configured using a file named `.mbeautifyrc` located in the current working directory (`pwd`). This allows you to set your preferred default options without specifying them in each function call.

### File Format

*   The file must be named `.mbeautifyrc`.
*   It's a plain text file.
*   Each line defines an option using a `key = value` format.
    *   Example: `IndentSize = 2`
*   Keys are case-insensitive (e.g., `indentsize` is treated as `IndentSize`).
*   Values should match the expected type for the option:
    *   **Numerics:** `IndentSize = 4`, `MinBlankLinesBeforeBlock = 1`
    *   **Logicals:** `UseTabs = true`, `SpaceAroundOperators = false` (case-insensitive 'true' or 'false')
    *   **Strings:** `OutputFormat = char`, `StylePreset = CompactStyle`
*   Lines starting with a hash symbol (`#`) are treated as comments and are ignored.
*   Blank lines are ignored.
*   Leading and trailing whitespace around keys and values are trimmed.

### Example `.mbeautifyrc`
```
# My preferred MATLAB beautifier settings

IndentSize = 2
UseTabs = false

SpaceAroundOperators = true
SpaceAfterComma = true

# StylePreset = CompactStyle
MinBlankLinesBeforeBlock = 1
```

### Option Precedence

The beautifier determines the final settings for each option based on the following order of precedence (highest to lowest):

1.  **Direct Arguments:** Options passed directly in the function call (e.g., `code_beautifier(code, 'IndentSize', 8)`). These override all other settings.
2.  **Style Preset:** Options defined by a `StylePreset` (e.g., `'MathWorksStyle'`, `'CompactStyle'`) if a preset is specified either as a direct argument or in the `.mbeautifyrc` file. If a preset is specified in both, the direct argument preset takes precedence.
3.  **`.mbeautifyrc` Options:** Options set in the `.mbeautifyrc` file.
4.  **Built-in Defaults:** The function's hardcoded default values (typically corresponding to the `'Default'` preset).

For instance, if `.mbeautifyrc` sets `IndentSize = 2`, but you call `code_beautifier(code, 'IndentSize', 4)`, the `IndentSize` will be 4. If `.mbeautifyrc` sets `IndentSize = 2` and you call `code_beautifier(code, 'StylePreset', 'MathWorksStyle')` (which defaults `IndentSize` to 4), the `IndentSize` will be 4.

## Examples

For a quick example of default formatting, consider this input:
```matlab
rawCode = {'function y=myfunc(x);if x > 0;;y=x*2+1;else;y=0;end;disp(y);end;'};
```

Applying `beautifulCode = code_beautifier(rawCode);` would result in (displayed from cell array):
```matlab
function y = myfunc(x)
    if x > 0;
        y = x * 2 + 1;
    else
        y = 0;
    end
    disp(y);
end
```
(Note: Semicolon after `if x > 0` might be added or removed based on specific semicolon options and MATLAB version context; example shows typical output suppression behavior.)

Further examples can be found in the main help text of the `code_beautifier.m` function itself.

## Known Limitations / Important Notes

*   The beautifier is based on regular expressions and heuristics, not a full MATLAB parser. While it aims for accuracy with common MATLAB syntax, it may not perfectly format all code, especially highly complex, heavily nested, or unusually styled constructs. Syntactically incorrect MATLAB code may also lead to unexpected formatting.
*   Performance might be a consideration for extremely large code files (e.g., tens of thousands of lines) due to line-by-line processing.
*   It's **highly recommended to use version control** (like Git) and review the changes made by the beautifier, especially when applying it to critical code or legacy projects.
*   The `AddSemicolonsToStatements` option is experimental and can significantly alter script output behavior by suppressing it. Use with caution and review its effects thoroughly.
*   Formatting of comments within code lines, especially complex trailing comments, aims for preservation but might have edge cases.

## Testing

A suite of unit tests (`test_code_beautifier.m`) is available to verify the formatter's behavior across various options and scenarios. Users can run these tests using MATLAB's unit testing framework to ensure the script functions as expected in their environment.

To run the tests:
```matlab
% Navigate to the directory containing test_code_beautifier.m
% Then run:
% results = runtests('test_code_beautifier');
% disp(results);
```

## Bug Reporting

If you encounter any issues or find a case where the beautifier does not format code as expected, please report it. When reporting a bug, include:
    - A minimal, reproducible example of the input code that causes the issue.
    - The beautifier options you were using (or if you were using a `.mbeautifyrc` file).
    - The actual output you received.
    - The output you expected.
This information will help in diagnosing and fixing the problem. (If this project is managed on a platform like GitHub, please open an issue in the project's issue tracker.)

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contributing

Contributions, bug reports, and suggestions are welcome! Please feel free to open an issue or submit a pull request on the repository (if this script is part of a version-controlled project). For local modifications, consider sharing improvements with colleagues or the community if appropriate.
