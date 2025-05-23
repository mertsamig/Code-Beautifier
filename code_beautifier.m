function beautifulCode = code_beautifier(rawCode, varargin)
% code_beautifier Formats MATLAB code for better readability.
%
%   beautifulCode = code_beautifier(rawCode)
%   Formats the input MATLAB code (string, cell array of strings, or string array)
%   using default settings. If `rawCode` is a string representing a path to an
%   existing `.m` file, the content of that file will be read and processed.
%
%   Optional Name-Value Pair Arguments:
%   'StylePreset':              String, applies a predefined set of styling options.
%                               Available presets:
%                                 'Default': Standard balanced style (default if no preset specified).
%                                 'MathWorksStyle': Emulates common MathWorks editor defaults
%                                                   with added spacing before blocks.
%                                 'CompactStyle': Prioritizes less vertical space with smaller indents
%                                                 and fewer blank lines.
%                               Individual options specified in the function call will override
%                               those set by the 'StylePreset'. Default: ''.
%   'IndentSize':               Scalar integer, number of spaces for one indent level.
%                               (e.g., from 'Default' preset: 4).
%   'UseTabs':                  Logical, true to use tabs for indentation.
%                               (e.g., from 'Default' preset: false).
%   'SpaceAroundOperators':     Logical, true to add spaces around binary operators.
%                               (e.g., from 'Default' preset: true).
%   'SpaceAfterComma':          Logical, true to add a space after commas.
%                               (e.g., from 'Default' preset: true).
%   'ContinuationIndentOffset': Scalar integer, additional indent levels for lines continued
%                               with '...'. (e.g., from 'Default' preset: 1).
%   'PreserveBlankLines':       Logical, true to keep single blank lines, collapse multiple;
%                               false to remove most blank lines.
%                               (e.g., from 'Default' preset: true).
%   'MinBlankLinesBeforeBlock': Scalar integer (0-2), ensures N blank lines before major
%                               block keywords (0 to disable). Max 2.
%                               (e.g., from 'Default' preset: 0).
%   'RemoveRedundantSemicolons':Logical, true to remove ';;' or 'end;'.
%                               (e.g., from 'Default' preset: true).
%   'AddSemicolonsToStatements':Logical, true to add semicolons to non-assignment
%                               function calls/expressions. (Use with caution).
%                               (e.g., from 'Default' preset: false).
%   'AlignAssignments':         Logical, true to align assignment statements within blocks
%                               of consecutive assignments. Default: false.
%   'FormatArgumentsBlock':     Logical, true to format 'arguments' blocks by aligning
%                               names, types, validation functions, and default values.
%                               Default: false.
%   'OutputFormat':             String, 'cell' (default) or 'char'. Defines output type.
%
% Example:
%   code = {'function y=myfunc(x);if x > 0;;y=x*2+1;else;y=0;end;disp(y);end;'};
%   % Using specific options:
%   prettyCell = code_beautifier(code, 'RemoveRedundantSemicolons', true, 'AddSemicolonsToStatements', true);
%   disp(strjoin(prettyCell, sprintf('\n')));
%
%   % Apply a preset:
%   compactCode = code_beautifier(code, 'StylePreset', 'CompactStyle');
%   disp(strjoin(compactCode, sprintf('\n')));
%
%   % Apply a preset and override one option:
%   customCompactCode = code_beautifier(code, 'StylePreset', 'CompactStyle', 'IndentSize', 4);
%   disp(strjoin(customCompactCode, sprintf('\n')));
%
% Example (File Input - assuming 'myScriptToFormat.m' exists):
%   % Create a dummy file for example:
%   % fid = fopen('myScriptToFormat.m', 'w');
%   % fprintf(fid, 'function test(arg1,arg2)\narguments\narg1 string="hello"\narg2 (1,1) double {mustBePositive}=10\nend\ndisp(arg1)\nend');
%   % fclose(fid);
%   formattedFromFile = code_beautifier('myScriptToFormat.m', 'FormatArgumentsBlock', true, 'OutputFormat', 'char');
%   disp(formattedFromFile);
%   % To save back to a new file (if output is 'char'):
%   % fidOut = fopen('myScriptFormatted.m', 'w');
%   % fprintf(fidOut, '%s', formattedFromFile);
%   % fclose(fidOut);
%   % delete('myScriptToFormat.m'); % Clean up dummy file
%
% Example (FormatArgumentsBlock):
%   argCode = {'arguments', '  firstArg string = "test"', '  secondArgument (1,1) double {mustBePositive} = 10', 'end'};
%   formattedArgs = code_beautifier(argCode, 'FormatArgumentsBlock', true, 'IndentSize', 2, 'OutputFormat', 'cell');
%   disp(strjoin(formattedArgs, sprintf('\n')));

    % --- Handle File Path Input for rawCode ---
    % This section checks if the primary input `rawCode` is a character string or
    % a scalar string that represents a path to an existing MATLAB file ('.m' extension).
    % If it is, the content of that file is read and used as the `rawCode` for beautification.
    % Otherwise, `rawCode` is treated as the direct code content (string, cell array, etc.).
    % Check if rawCode might be a file path
    if (ischar(rawCode) || (isstring(rawCode) && isscalar(rawCode)))
        inputPath = char(rawCode); % Convert to char for file operations
        % Check if it's an existing file and ends with .m
        if exist(inputPath, 'file') == 2 && endsWith(lower(inputPath), '.m')
            try
                rawCode = fileread(inputPath); % Read content from file
            catch ME
                error('code_beautifier:FileReadError', 'Failed to read input file "%s": %s', inputPath, ME.message);
            end
            % If it was a file path but not .m or not found, rawCode remains as is,
            % and will be treated as code content by the parser.
        end
    end

    % --- Style Presets Definition ---
    stylePresets = struct();
    stylePresets.Default = struct(...
        'IndentSize', 4, ...
        'UseTabs', false, ...
        'SpaceAroundOperators', true, ...
        'SpaceAfterComma', true, ...
        'ContinuationIndentOffset', 1, ...
        'PreserveBlankLines', true, ...
        'MinBlankLinesBeforeBlock', 0, ...
        'RemoveRedundantSemicolons', true, ...
        'AddSemicolonsToStatements', false, ...
        'AlignAssignments', false, ...
        'FormatArgumentsBlock', false ...
    );
    stylePresets.MathWorksStyle = struct(...
        'IndentSize', 4, ...
        'UseTabs', false, ...
        'SpaceAroundOperators', true, ...
        'SpaceAfterComma', true, ...
        'ContinuationIndentOffset', 1, ...
        'PreserveBlankLines', true, ...
        'MinBlankLinesBeforeBlock', 1, ...
        'RemoveRedundantSemicolons', true, ...
        'AddSemicolonsToStatements', false, ...
        'AlignAssignments', false, ...
        'FormatArgumentsBlock', false ...
    );
    stylePresets.CompactStyle = struct(...
        'IndentSize', 2, ...
        'UseTabs', false, ...
        'SpaceAroundOperators', true, ...
        'SpaceAfterComma', true, ...
        'ContinuationIndentOffset', 1, ...
        'PreserveBlankLines', false, ...
        'MinBlankLinesBeforeBlock', 0, ...
        'RemoveRedundantSemicolons', true, ...
        'AddSemicolonsToStatements', false, ...
        'AlignAssignments', false, ...
        'FormatArgumentsBlock', false ...
    );

    % --- Determine Effective Defaults (Precedence: Function Defaults -> Config File -> Preset -> Direct Args) ---
    % The effective options are determined in a specific order of precedence:
    % 1. Hardcoded Defaults: These are the base settings defined in `stylePresets.Default`.
    % 2. Config File Options: Settings loaded from `.mbeautifyrc` override hardcoded defaults.
    % 3. Style Preset: A chosen style preset (either from config file or direct argument)
    %    overrides settings from step 1 and 2. If specified in both, direct argument preset wins.
    % 4. Direct Arguments: Options passed directly to `code_beautifier` override all previous settings.

    % 1. Start with hardcoded function defaults (via stylePresets.Default)
    % `effectiveDefaults` will be progressively updated to reflect the applied settings at each stage.
    % Initially, it holds the most basic defaults.
    effectiveDefaults = stylePresets.Default;
    
    % 2. Load and Overlay Config File Options
    % Load options from `.mbeautifyrc` if it exists in the current working directory. 
    % These options will override the initial hardcoded defaults.
    % `knownOptionsInfo` provides metadata about each option (e.g., type) for parsing.
    knownOptionsInfo = getKnownOptionsInfo(stylePresets.Default); % Get type info for parsing config file values.
    configFilePath = fullfile(pwd, '.mbeautifyrc'); % Define the path to the configuration file.
    configFileOptions = struct(); % Initialize a struct to hold options loaded from the config file.
    if exist(configFilePath, 'file') % Check if the config file exists.
        configFileOptions = parseConfigFile(configFilePath, knownOptionsInfo); % Parse the file.
        % Overlay options from the config file onto the current `effectiveDefaults`.
        % If an option from the config file is also in `effectiveDefaults`, its value is updated.
        fieldsToUpdate = fieldnames(configFileOptions);
        for k_f = 1:length(fieldsToUpdate)
            fieldName = fieldsToUpdate{k_f};
            if isfield(effectiveDefaults, fieldName) % Ensure the option is a known and valid one.
                effectiveDefaults.(fieldName) = configFileOptions.(fieldName);
            end
        end
    end

    % 3. Determine and Overlay StylePreset Options
    %    A StylePreset can be specified either in the `.mbeautifyrc` config file or as a direct
    %    argument to the `code_beautifier` function. 
    %    The direct argument for 'StylePreset' takes precedence if both are provided.
    %    Once the `finalPresetName` is determined, its associated settings (from `stylePresets` struct)
    %    override any corresponding settings currently in `effectiveDefaults`.
    
    % Check varargin (direct function arguments) for 'StylePreset' first, as it has higher precedence.
    directArgPresetName = ''; % Stores the preset name if provided as a direct argument.
    for k_v = 1:2:length(varargin) % Iterate through Name-Value pairs in varargin.
        if strcmpi(varargin{k_v}, 'StylePreset') && k_v + 1 <= length(varargin)
            directArgPresetName = char(varargin{k_v+1}); % Found 'StylePreset' in direct args.
            break;
        end
    end
    
    % `finalPresetName` will store the name of the preset that will ultimately be applied.
    % It's determined by checking direct arguments first, then the config file options.
    finalPresetName = '';
    if ~isempty(directArgPresetName)
        % A preset name was provided as a direct argument. This takes precedence.
        % Actual validation of this name (i.e., whether it's a known/valid preset like 'Default', 'MathWorksStyle')
        % will occur later during the main input parsing stage (using `validateStylePreset`).
        finalPresetName = directArgPresetName;
    elseif isfield(configFileOptions, 'StylePreset') && ~isempty(configFileOptions.StylePreset)
        % No direct 'StylePreset' argument, so check if one was specified in the config file.
        % Validate that this preset name from the config file is one of the known, valid preset names.
        try
            validateStylePreset(configFileOptions.StylePreset); % This function checks against known preset names.
            finalPresetName = char(configFileOptions.StylePreset); % If valid, use this preset name.
        catch ME
            % If `validateStylePreset` throws an error (meaning the name is invalid),
            % issue a warning and ignore the invalid preset from the config file.
            warning('code_beautifier:InvalidStylePresetInConfigFile', ...
                    'Invalid StylePreset "%s" in .mbeautifyrc: %s. Ignoring this preset.', ...
                    configFileOptions.StylePreset, ME.message);
        end
    end

    % If a `finalPresetName` has been determined (either from a direct argument or a valid config file entry),
    % apply its settings by overlaying them onto `effectiveDefaults`.
    if ~isempty(finalPresetName)
        % Ensure `finalPresetName` corresponds to one of the defined preset structures
        % (e.g., `stylePresets.Default`, `stylePresets.MathWorksStyle`).
        % This also canonicalizes the casing of `finalPresetName` (e.g., 'default' becomes 'Default').
        validPresetNames = fieldnames(stylePresets); % Get names of all defined preset structures.
        isKnownPreset = false; % Flag to track if `finalPresetName` matches a known preset.
        for vp_idx = 1:length(validPresetNames)
            if strcmpi(finalPresetName, validPresetNames{vp_idx}) % Case-insensitive matching.
                finalPresetName = validPresetNames{vp_idx}; % Use the canonical casing (e.g., 'Default').
                isKnownPreset = true;
                break;
            end
        end

        if isKnownPreset
            % If `finalPresetName` is a known and valid preset, retrieve its settings
            % from the `stylePresets` struct and apply them, overriding current `effectiveDefaults`.
            presetSettingsToApply = stylePresets.(finalPresetName);
            fieldsToUpdate = fieldnames(presetSettingsToApply);
            for k_f = 1:length(fieldsToUpdate)
                fieldName = fieldsToUpdate{k_f};
                effectiveDefaults.(fieldName) = presetSettingsToApply.(fieldName);
            end
        elseif ~isempty(directArgPresetName) && strcmpi(directArgPresetName, finalPresetName)
            % This case occurs if `directArgPresetName` was given (e.g., 'myownstyle') but it's not
            % among the `stylePresets` keys. The main `inputParser` will later catch this as an
            % invalid 'StylePreset' value if `validateStylePreset` is correctly set up for the parameter.
            % No warning is explicitly issued here to avoid redundancy with the inputParser's error.
        elseif ~isempty(finalPresetName) 
            % This branch might be reached if `finalPresetName` was set from an invalid config file preset
            % (which should have already generated a warning) and was not overridden by a direct argument.
            % The main input parser will ultimately handle any invalid preset name that reaches it.
        end
    end

    % --- Input Parsing Setup (using effectiveDefaults) ---
    % The `inputParser` is now initialized. The default values for each parameter are drawn from
    % `effectiveDefaults`. At this stage, `effectiveDefaults` contains settings derived from:
    %   1st: Hardcoded Defaults (from `stylePresets.Default`)
    %   2nd: Overridden by `.mbeautifyrc` Config File Options (if file exists and options are valid)
    %   3rd: Overridden by a Chosen Style Preset's settings (if a preset was specified in config or direct args and was valid)
    %
    % Any options passed directly as arguments in `varargin` to `code_beautifier` will, during the `parse()` call,
    % override these `effectiveDefaults`. This completes the specified order of precedence:
    % Hardcoded -> Config File -> Style Preset -> Direct Arguments.
    p = inputParser;
    addRequired(p, 'rawCode', @(x) ischar(x) || iscellstr(x) || isstring(x)); % The input code itself.
    
    % Add all beautifier options as parameters to the input parser.
    % Their default values are set from `effectiveDefaults` which have been built up.
    % Each parameter also has a validation function to ensure its type and range are correct.
    addParameter(p, 'StylePreset', effectiveDefaults.StylePreset, @validateStylePreset); % Note: 'StylePreset' itself is an option that influences other defaults.
    addParameter(p, 'IndentSize', effectiveDefaults.IndentSize, @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x);
    addParameter(p, 'UseTabs', effectiveDefaults.UseTabs, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'SpaceAroundOperators', effectiveDefaults.SpaceAroundOperators, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'SpaceAfterComma', effectiveDefaults.SpaceAfterComma, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'ContinuationIndentOffset', effectiveDefaults.ContinuationIndentOffset, @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x);
    addParameter(p, 'PreserveBlankLines', effectiveDefaults.PreserveBlankLines, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'MinBlankLinesBeforeBlock', effectiveDefaults.MinBlankLinesBeforeBlock, @(x) isnumeric(x) && isscalar(x) && x >= 0 && x <=2 && floor(x) == x);
    addParameter(p, 'RemoveRedundantSemicolons', effectiveDefaults.RemoveRedundantSemicolons, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'AddSemicolonsToStatements', effectiveDefaults.AddSemicolonsToStatements, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'AlignAssignments', effectiveDefaults.AlignAssignments, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'FormatArgumentsBlock', effectiveDefaults.FormatArgumentsBlock, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'OutputFormat', effectiveDefaults.OutputFormat, @(x) (ischar(x) || (isstring(x) && isscalar(x))) && ismember(lower(char(x)), {'cell', 'char'}));

    % 4. Parse Direct Arguments (varargin). These arguments have the highest precedence.
    %    The inputParser takes `rawCode` (the required argument) and `varargin` (optional Name-Value pairs).
    %    Any Name-Value pairs in `varargin` that match defined parameters will override the defaults
    %    that were previously set in `effectiveDefaults`.
    %    This step finalizes the option precedence: Hardcoded -> Config File -> Style Preset -> Direct Arguments.
    parse(p, rawCode, varargin{:});
    options = p.Results; % `options` now holds the final, fully resolved settings to be used by the beautifier.
    
    % Final check for 'StylePreset' canonical casing within the fully resolved `options`.
    % If a 'StylePreset' was provided as a direct argument (e.g., 'compactstyle' instead of 'CompactStyle')
    % and it successfully matched a known preset (due to case-insensitive validation),
    % this ensures that `options.StylePreset` stores the canonical form (e.g., 'CompactStyle').
    % This is mainly for internal consistency if `options.StylePreset` is directly queried later.
    if ~isempty(options.StylePreset)
        currentPresetVal = char(options.StylePreset);
        validPresetNames = fieldnames(stylePresets); % Canonical names are defined in `stylePresets`.
        for vp_idx = 1:length(validPresetNames)
            if strcmpi(currentPresetVal, validPresetNames{vp_idx}) % Case-insensitive check against canonical names.
                options.StylePreset = validPresetNames{vp_idx}; % Ensure `options.StylePreset` uses the canonical casing.
                break;
            end
        end
    end

    % Determine the character(s) to use for indentation based on the final options.
    if options.UseTabs
        indentChar = sprintf('\t'); % Use a literal tab character if `UseTabs` is true.
        indentUnit = 1;             % Each indent level corresponds to one tab character.
    else
        indentChar = ' '; % Use space characters if `UseTabs` is false.
        indentUnit = options.IndentSize; % The number of spaces per indent level is defined by `IndentSize`.
    end

    % Convert input `rawCode` (which can be char, string array, or cell array of strings) 
    % into a standardized cell array of strings (`lines`), where each cell represents one line of code.
    % This allows uniform line-by-line processing later.
    if ischar(rawCode) % If input is a single character array (potentially containing multiple lines via \n).
        lines = strsplit(rawCode, {'\r\n', '\n', '\r'}, 'CollapseDelimiters', false)'; % Split by various newline sequences.
    elseif isstring(rawCode) % If input is a MATLAB string array (R2016b+).
        if isscalar(rawCode) % If it's a single string element (can also contain multiple lines).
             lines = strsplit(rawCode, {'\r\n', '\n', '\r'}, 'CollapseDelimiters', false)';
        else % If it's a string array where each element is intended to be a line.
            lines = cellstr(rawCode); % Convert to a cell array of character vectors.
        end
    else % Assumed to be a cell array of strings (cellstr), which is already in the desired format.
        lines = rawCode;
    end

    % --- Keywords Definitions ---
    % Define MATLAB keywords that influence indentation logic.
    indentKeywords     = {'if', 'for', 'while', 'switch', 'try', 'parfor', 'function', 'classdef', 'properties', 'methods', 'events', 'arguments'}; % Keywords that start an indented block.
    dedentKeywords     = {'end'}; % Keywords that end an indented block.
    midBlockKeywords   = {'elseif', 'else', 'catch', 'case', 'otherwise'}; % Keywords that occur mid-block, often dedenting then indenting.
    allBlockCtrlKeywords = [indentKeywords, dedentKeywords, midBlockKeywords]; % Combined list of all control keywords.
    firstWordPattern   = ['^\s*(', strjoin(allBlockCtrlKeywords, '|'), ')\b']; % Regex pattern to efficiently find the first control keyword on a line.

    % --- Main Processing Loop ---
    % This loop iterates through each line of the input code (`lines`) to apply formatting rules.
    % Key stages for each line include:
    % 1. Block Comment Handling (%{ ... %}): These are passed through with base indentation.
    % 2. Empty Line Handling: Initially marked, final processing occurs in post-processing stage.
    % 3. Code/Comment Extraction: Separates the functional code part of a line from its trailing comment (e.g., "x = 1; % comment").
    % 4. Indentation Logic: Determines the correct indentation level for the current line based on:
    %    - Control keywords (if, for, end, etc.).
    %    - Whether the line is a continuation of the previous line (ends with '...').
    %    - Special context for 'switch' and 'case' statements.
    % 5. Spacing and Semicolon Logic:
    %    - Adds or removes spaces around operators (e.g., '+', '=', '==').
    %    - Ensures consistent spacing after commas.
    %    - Manages semicolons: removing redundant ones or (optionally) adding them to suppress output.
    % 6. Constructing the Beautiful Line: Assembles the indented code part and the comment part.
    % 7. Updating IndentLevel for Next Line: Adjusts the base `indentLevel` for subsequent lines based on keywords found on the current line.

    indentLevel = 0; % Current base indentation level (number of indent units).
    tempBeautifulLines = cell(size(lines)); % Pre-allocate cell array for the processed lines.
    inBlockComment = false; % Flag: true if currently processing lines within a %{ ... %} block comment.
    previousLineEndedWithContinuation = false; % Flag: true if the *previous* processed line ended with '...'.
    previousLineActualIndentStr = ''; % Stores the actual indent string (spaces/tabs) of the previously processed code/comment line.
                                      % Used for aligning continuation lines relative to the previous line's content.
    
    inSwitchBlockDepth = 0; % Counter for nested 'switch' statements. Helps manage indentation for 'case' and 'otherwise'.
                            % Each 'switch' increments it, each 'end' (assumed to close a switch) decrements it.
    inCaseBody = false;     % Flag: true if currently processing lines that are part of a 'case' or 'otherwise' block's body
                            % (i.e., the lines indented underneath a 'case' or 'otherwise' keyword).
                            % This flag triggers an additional indent level for these lines.

    for i = 1:length(lines) % Loop through each line of the input code.
        originalLine = lines{i}; % The raw, original line.
        trimmedOriginalLine = strtrim(originalLine); % Line with leading/trailing whitespace removed

        % --- Stage 1: Handle Block Comments %{ ... %} ---
        % Block comments are preserved with their base indentation matching the current `indentLevel`.
        % Original relative indentation within the block comment is maintained.
        if startsWith(trimmedOriginalLine, '%{') % Start of a block comment
            inBlockComment = true;
            baseIndentStr = repmat(indentChar, 1, indentLevel * indentUnit * (options.IndentSize > 0) );
            tempBeautifulLines{i} = [baseIndentStr, trimmedOriginalLine]; % Apply base indent
            previousLineEndedWithContinuation = false;
            continue; % Move to next line
        elseif inBlockComment % Inside a block comment
            baseIndentStr = repmat(indentChar, 1, indentLevel * indentUnit * (options.IndentSize > 0) );
            if endsWith(trimmedOriginalLine, '%}') % End of a block comment
                inBlockComment = false;
            end
            % Preserve original leading spaces for content lines within the block comment, after applying base indent.
            tempBeautifulLines{i} = [baseIndentStr, originalLine]; 
            previousLineEndedWithContinuation = false;
            continue; % Move to next line
        end

        % --- Stage 2: Handle Empty Lines ---
        % Empty or whitespace-only lines are marked as empty for now.
        % Final handling of blank lines (preserving/removing) happens in post-processing.
        if isempty(trimmedOriginalLine)
            tempBeautifulLines{i} = ''; 
            previousLineEndedWithContinuation = false;
            continue; % Move to next line
        end
        
        % --- Stage 3: Extract Code and Comment Parts ---
        % Separates the executable code from the trailing line comment (e.g., "code; % comment").
        % Handles cases where '%' might be part of a string literal.
        [codePart, commentPart] = extractCodeAndCommentInternal(trimmedOriginalLine);
        
        % --- Determine First Word (Keyword) for Indentation ---
        % Check if the first word in the code part is a control keyword.
        firstWordToken = regexp(codePart, firstWordPattern, 'tokens', 'once');
        if ~isempty(firstWordToken), firstWord = firstWordToken{1}; else, firstWord = ''; end

        % --- Line is Only a Comment (after block comments and empty lines are handled) ---
        % If `codePart` is empty, the line is treated as a full-line comment.
        if isempty(codePart) && ~isempty(commentPart) % True if line is effectively comment-only
            currentLineEffectiveIndentLevel = indentLevel;
            % If inside a 'case' body, comments are indented further to align with case content.
            if inCaseBody 
                 currentLineEffectiveIndentLevel = currentLineEffectiveIndentLevel + 1;
            end
            currentIndentStr = repmat(indentChar, 1, currentLineEffectiveIndentLevel * indentUnit * (options.IndentSize > 0));
            
            % If this comment line is a continuation of a previous code line (e.g. code ... % comment part 1
            %                                                                  % comment part 2)
            % then its indent should be based on the continued line's indent.
            if previousLineEndedWithContinuation 
                currentIndentStr = [previousLineActualIndentStr, ... % Start with previous line's actual code indent
                                    repmat(indentChar, 1, options.ContinuationIndentOffset * indentUnit * (options.IndentSize > 0))]; % Add continuation offset
            end
            tempBeautifulLines{i} = regexprep([currentIndentStr, commentPart], '\s+$', ''); % Add indent and trim trailing space
            previousLineEndedWithContinuation = false; 
            previousLineActualIndentStr = currentIndentStr; % Store this comment's indent for potential next continuation
            continue; % Move to next line
        end

        % --- Stage 4: Indentation Logic for Current Code Line (that contains code) ---
        currentLineEffectiveIndentLevel = indentLevel; % Default indent level for this code line

        if ismember(firstWord, dedentKeywords) % Handles 'end'
            currentLineEffectiveIndentLevel = max(0, indentLevel - 1); % 'end' is dedented
            % State changes for `inSwitchBlockDepth` and `inCaseBody` when an 'end'
            % closes a switch are handled in the "Update IndentLevel for NEXT line" section.
        elseif ismember(firstWord, midBlockKeywords) % Handles 'elseif', 'else', 'catch', 'case', 'otherwise'
            if ismember(firstWord, {'case', 'otherwise'})
                % 'case' and 'otherwise' keywords are indented to the current `indentLevel`,
                % which is the level of the 'switch' statement's content.
                % `currentLineEffectiveIndentLevel` remains `indentLevel`.
                inCaseBody = true; % Set flag: subsequent lines are content of this case/otherwise.
            else % 'elseif', 'else', 'catch'
                % These keywords are dedented one level relative to the block they are part of.
                currentLineEffectiveIndentLevel = max(0, indentLevel - 1);
                inCaseBody = false; % Reset flag, not in a 'case' body for if/try blocks.
            end
        end
        
        % Additional indentation for statements that are *inside* a 'case' or 'otherwise' body.
        % This applies if `inCaseBody` is true (set by 'case' or 'otherwise' keyword)
        % AND the current line is not itself a block control keyword (like 'if' nested in 'case').
        if inCaseBody && ~ismember(firstWord, allBlockCtrlKeywords) && ~isempty(firstWord) % `~isempty(firstWord)` ensures it's not just a comment
             currentLineEffectiveIndentLevel = currentLineEffectiveIndentLevel + 1; % Indent case content further
        end

        % Determine the indent string based on the calculated effective level.
        currentIndentStr = repmat(indentChar, 1, currentLineEffectiveIndentLevel * indentUnit * (options.IndentSize > 0));

        % If this line is a continuation of the previous line.
        if previousLineEndedWithContinuation
            % The indent string is based on the previous line's actual code indent plus continuation offset.
            currentIndentStr = [previousLineActualIndentStr, ... % Start with previous line's code indent
                                repmat(indentChar, 1, options.ContinuationIndentOffset * indentUnit * (options.IndentSize > 0))]; % Add offset
        end
        previousLineActualIndentStr = currentIndentStr; % Store this line's code indent for any potential next continuation line.

        % --- Stage 5: Spacing and Semicolon Logic (Applied to codePart) ---
        processedCodePart = codePart; % Start with the extracted code part
        if ~isempty(processedCodePart)
            % Semicolon Management (applied before spacing operators, as it might affect line end)
            if options.RemoveRedundantSemicolons
                processedCodePart = regexprep(processedCodePart, ';(\s*;)+', ';'); % Replace ';;' or more with a single ';'
                % Remove semicolon after 'end' if it's not syntactically required (e.g., not `end);` for anonymous functions)
                if strcmp(firstWord, 'end') && endsWith(strtrim(processedCodePart), ';')
                    tempTrimmed = strtrim(processedCodePart);
                    if ~ismember(tempTrimmed(end-1), {')', ']', '}'}) % Avoid changing `end);` to `end)`
                         processedCodePart = strtrim(tempTrimmed(1:end-1)); % Remove the semicolon
                    end
                end
            end

            if options.AddSemicolonsToStatements % Experimental feature
                % Determine if the line looks like a statement that would print to command window if no semicolon.
                isAssignment = ~isempty(regexp(processedCodePart, '(?<![=<>~.\s])=(?![=])', 'once')); % True if line contains an assignment (not ==, <= etc.)
                isKeywordLine = ~isempty(firstWord); % True if line starts with a control keyword
                endsWithContOrSemi = endsWith(strtrim(processedCodePart), '...') || endsWith(strtrim(processedCodePart), ';'); % True if ends with ... or ;
                
                % Heuristic: Add semicolon if it looks like a function call or simple expression,
                % and is NOT an assignment, NOT a keyword line, and NOT already ending with ';' or '...'.
                isFunctionCallLike = ~isempty(regexp(processedCodePart, '\w\s*\(.*\)', 'once')); % e.g., func() or obj.method()
                isSimpleExpression = ~isempty(regexp(processedCodePart, '\w', 'once')) && ... % Contains some word characters
                                     isempty(regexp(processedCodePart,'^\s*\w+\s*$', 'once')); % Is not just a single variable name
                                     
                if ~isAssignment && ~isKeywordLine && ~endsWithContOrSemi && (isFunctionCallLike || isSimpleExpression)
                    processedCodePart = [processedCodePart, ';']; % Append semicolon
                end
            end

            % Operator Spacing
            if options.SpaceAroundOperators
                % General operators (excluding +,- which need context, and element-wise handled by .*, ./ etc.)
                opListGeneral = { ...
                    '==', '~=', '<=', '>=', '&&', '||', ... % Comparison & Logical operators
                    '.*', './', '.\\', '.^', ...             % Element-wise arithmetic operators
                    '*', '/', '\\', '^', ...                 % Other arithmetic operators (in binary context)
                    '=' ...                                  % Assignment operator
                    };
                % Iterate to ensure longer operators (e.g., '==') are processed before shorter ones (e.g., '=')
                % to prevent incorrect spacing.
                for op_idx = 1:length(opListGeneral)
                    op = opListGeneral{op_idx};
                    escaped_op = regexptranslate('escape', op); % Escape special regex characters in operator
                    % Pattern `pat`: Catches an operator if it's surrounded by non-whitespace characters,
                    % with optional existing spaces.
                    % (\S): Captures a non-whitespace character (token before operator).
                    % \s*: Matches zero or more whitespace characters.
                    % $1, $2: Backreferences to the captured non-whitespace characters.
                    pat = ['(\S)\s*', escaped_op, '\s*(\S)']; 
                    rep = ['$1 ', op, ' $2']; % Replaces with: token1<space>operator<space>token2
                    processedCodePart = regexprep(processedCodePart, pat, rep);
                end

                % Special handling for binary + and - operators to distinguish from unary.
                % `s1`: Captures a valid character that can precede a binary operator (+ or -).
                %       (word character, or closing bracket/paren, or transpose quote).
                % `s2`: Captures a valid character that can follow a binary operator.
                %       (word character, or opening bracket/paren, or dot for properties/element-wise).
                % `pat_binary_plus_minus`: Matches pattern <char_before><optional_spaces><+/-><optional_spaces><char_after>.
                s1 = '(\w|\)|\]|\'')'; % Capture group 1: character before operator
                s2 = '(\w|\(|\[|\.)'; % Capture group 3: character after operator
                pat_binary_plus_minus = [s1, '\s*([+\-])\s*', s2]; % Capture group 2: the operator + or -
                rep_binary_plus_minus = '$1 $2 $3'; % Add single spaces around the operator
                processedCodePart = regexprep(processedCodePart, pat_binary_plus_minus, rep_binary_plus_minus);
                
                % Unary plus/minus and scientific notation fixes (post-general spacing)
                % Fix 1: Remove space after specific preceding tokens if followed by unary +/-.
                % `unary_fix_class_1`: Characters that can precede a unary operator (e.g., '(', '[', '=', etc.).
                % `pat_unary_fix_1`: Matches <preceding_char><one_or_more_spaces><+/-><operand_char>.
                % Result: <preceding_char><+/-><operand_char> (removes the unwanted space).
                unary_fix_class_1 = '[=\(\[\{,\s&|]'; 
                pat_unary_fix_1 = ['(', unary_fix_class_1, ')\s+([+\-])\s*(\w|[\.\(])'];
                processedCodePart = regexprep(processedCodePart, pat_unary_fix_1, '$1$2$3');
                
                % Fix 2: Remove space if unary +/- is at the beginning of the code part (after indent).
                % `pat_unary_fix_2`: Matches <start_of_code><+/-><one_or_more_spaces><operand_char>.
                pat_unary_fix_2 = ['^([+\-])\s+(\w|[\.\(])'];
                processedCodePart = regexprep(processedCodePart, pat_unary_fix_2, '$1$2');
                
                % Fix scientific notation (e.g., "1 e - 5" -> "1e-5", or "1e + 5" -> "1e+5").
                % Handles cases where spaces might have been inserted around 'e'/'E' or the exponent's sign.
                processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*([+\-])\s*(\d+)', '$1e$2$3', 'ignorecase'); % e.g. 1e-5 or 1E+10
                processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*(\d+)', '$1e$2', 'ignorecase'); % For exponents without explicit sign, e.g. 1e10
            end

            % Space after comma
            if options.SpaceAfterComma
                processedCodePart = regexprep(processedCodePart, '\s*,\s*', ', '); % Ensures one space after comma, removes spaces before.
                processedCodePart = regexprep(processedCodePart, ', $', ','); % Removes trailing space if comma is at end of code part.
            end
            
            % Ensure space after a semicolon if it's used as a separator within a statement (e.g., in matrix definitions like `[1; 2]`)
            processedCodePart = regexprep(processedCodePart, ';(\S)', '; $1'); 
        end

        % --- Stage 6: Construct the Beautiful Line ---
        % Assemble the indented code part and the (optional) comment part.
        % Handles cases: only comment, code + comment, only code.
        if isempty(strtrim(processedCodePart)) && ~isempty(commentPart) % Line became comment-only after processing
            tempBeautifulLines{i} = regexprep([currentIndentStr, commentPart], '\s+$', '');
        elseif ~isempty(strtrim(processedCodePart)) && ~isempty(commentPart) % Code and comment
            tempBeautifulLines{i} = regexprep([currentIndentStr, strtrim(processedCodePart), commentPart], '\s+$', '');
        elseif ~isempty(strtrim(processedCodePart)) % Only code
            tempBeautifulLines{i} = regexprep([currentIndentStr, strtrim(processedCodePart)], '\s+$', '');
        else % Line became effectively empty (e.g. was just whitespace or processed to empty)
            tempBeautifulLines{i} = ''; 
        end
        
        % --- Stage 7: Update IndentLevel for NEXT line ---
        if options.IndentSize > 0 % Only adjust indent level if indenting is active (IndentSize > 0)
            if ismember(firstWord, dedentKeywords) % Keyword is 'end'
                current_indentLevel_before_dedent = indentLevel;
                indentLevel = max(0, indentLevel - 1); % Dedent for the 'end'
                
                % Special handling for 'end' closing a 'switch' block.
                if inSwitchBlockDepth > 0 % Currently inside one or more switch blocks
                    % This is a simplified model assuming 'end' closes the innermost 'switch'.
                    % A robust parser would use a stack to match 'end' to 'switch'.
                    if indentLevel < current_indentLevel_before_dedent % If actual dedent happened
                        % This 'end' is assumed to close the current switch context.
                        inSwitchBlockDepth = max(0, inSwitchBlockDepth - 1);
                        if inSwitchBlockDepth == 0 % If exited all nested switch blocks
                            inCaseBody = false; % Reset 'inCaseBody' flag.
                        end
                    end
                end
            elseif ismember(firstWord, midBlockKeywords) % Keywords like 'elseif', 'else', 'case'
                if ismember(firstWord, {'case', 'otherwise'})
                    % `indentLevel` for lines *after* 'case'/'otherwise' does not change here.
                    % It's already at the switch content level. The content *within* the case
                    % is handled by `inCaseBody` flag, which causes `currentLineEffectiveIndentLevel`
                    % to be increased for those content lines.
                else % 'elseif', 'else', 'catch'
                    % These keywords first conceptually "dedent" to the level of the 'if'/'try'
                    % then "indent" for their own block content. Net effect on `indentLevel` for
                    % the *next* line is typically no change from the start of their block.
                    % `currentLineEffectiveIndentLevel` for these keywords themselves was already set.
                    % The `indentLevel` for the *next* line (content of else/elseif/catch) should be `currentLineEffectiveIndentLevel + 1`.
                    % This is achieved by: dedent for keyword, then indent for block.
                    indentLevel = max(0, indentLevel - 1); % Dedent part for the keyword itself.
                    indentLevel = indentLevel + 1;         % Indent part for the content of the block.
                end
            elseif ismember(firstWord, indentKeywords) % Keywords like 'if', 'for', 'function', 'switch'
                if strcmp(firstWord, 'switch')
                    inSwitchBlockDepth = inSwitchBlockDepth + 1; % Increment switch nesting depth
                end
                indentLevel = indentLevel + 1; % Indent for the block content
            end
        end
        previousLineEndedWithContinuation = endsWith(strtrim(processedCodePart), '...'); % Check for line continuation for next iteration
    end

    % --- Post Processing: Blank Lines and MinBlankLinesBeforeBlock ---
    % Refine blank lines based on options.PreserveBlankLines and options.MinBlankLinesBeforeBlock.
    finalOutputLines = cell(1, length(tempBeautifulLines) + options.MinBlankLinesBeforeBlock * length(tempBeautifulLines)); % Pre-allocate, possibly overestimate
    finalLineCount = 0;
    lastMeaningfulLineWasBlank = true; % True if the last non-empty line added to finalOutputLines was a blank line (or start of file).

    for k = 1:length(tempBeautifulLines) % Iterate through the initially processed lines
        currentLineContent = strtrim(tempBeautifulLines{k}); % Get content of current processed line
        isCurrentLineBlank = isempty(currentLineContent); % Check if it's blank after initial processing

        % MinBlankLinesBeforeBlock logic: Ensure N blank lines before major block keywords.
        % Only apply if MinBlankLinesBeforeBlock > 0, current line is not blank, and not at the very start of the file.
        if options.MinBlankLinesBeforeBlock > 0 && ~isCurrentLineBlank && finalLineCount > 0
            % Check if current line starts a new block (e.g., 'if', 'for', 'function')
            [codeP_for_blank_check, ~] = extractCodeAndCommentInternal(currentLineContent); % Simple re-extract for keyword check
            firstWordToken_for_blank_check = regexp(codeP_for_blank_check, ['^\s*(', strjoin(indentKeywords, '|'), ')\b'], 'tokens', 'once');
            
            if ~isempty(firstWordToken_for_blank_check) % It's a block-starting keyword
                blanksNeeded = options.MinBlankLinesBeforeBlock;
                % Count existing blank lines immediately preceding this point in `finalOutputLines`.
                numExistingBlanks = 0;
                if finalLineCount > 0
                    for j = finalLineCount:-1:1 % Look backwards from last added line
                        if isempty(strtrim(finalOutputLines{j}))
                            numExistingBlanks = numExistingBlanks + 1;
                        else
                            break; % Hit a non-blank line
                        end
                    end
                end
                
                % Add missing blank lines.
                for bl = 1:max(0, blanksNeeded - numExistingBlanks)
                    finalLineCount = finalLineCount + 1;
                    finalOutputLines{finalLineCount} = '';
                end
            end
        end

        % PreserveBlankLines logic
        if isCurrentLineBlank
            if options.PreserveBlankLines % If preserving blank lines
                if ~lastMeaningfulLineWasBlank % And the last meaningful line wasn't also blank (to collapse multiple to one)
                    finalLineCount = finalLineCount + 1;
                    finalOutputLines{finalLineCount} = ''; % Add the blank line
                    lastMeaningfulLineWasBlank = true;
                end
                % else: current is blank, and previous was also blank (or start of file), so collapse/skip.
            else
                % Not preserving blank lines, so skip adding this blank line.
            end
        else % Current line is not blank
            finalLineCount = finalLineCount + 1;
            finalOutputLines{finalLineCount} = tempBeautifulLines{k}; % Add the contentful line
            lastMeaningfulLineWasBlank = false;
        end
    end
    beautifulLines = finalOutputLines(1:finalLineCount)'; % Trim pre-allocated cell array

    % --- Optional: Align Assignments ---
    % If enabled, this step realigns blocks of consecutive assignment statements.
    if options.AlignAssignments && ~isempty(beautifulLines)
        beautifulLines = alignAssignmentBlocksInternal(beautifulLines, options);
    end

    % --- Output Formatting ---
    % Convert the cell array of processed lines back to the requested output format ('char' or 'cell').
    if strcmpi(options.OutputFormat, 'char')
        beautifulCode = strjoin(beautifulLines, sprintf('\n')); % Join lines into a single char array with '\n' separators
    else % 'cell' (default)
        beautifulCode = beautifulLines; % Return as cell array of strings
    end
end

% --- Helper function to format 'arguments' blocks ---
function formattedBlockLines = formatArgumentsBlockInternal(blockLines, options, indentChar, indentUnit)
    % formatArgumentsBlockInternal Parses and formats lines within an 'arguments' block for alignment.
    %
    % Syntax:
    %   formattedBlockLines = formatArgumentsBlockInternal(blockLines, options, indentChar, indentUnit)
    %
    % Inputs:
    %   blockLines: Cell array of strings. Each cell contains one line from the
    %               'arguments' block (excluding the 'arguments' and 'end' lines themselves).
    %   options:    Struct containing the beautifier options (e.g., SpaceAroundOperators).
    %   indentChar: Character string used for a single indentation unit (e.g., '    ' or '\t').
    %               This is not directly used for indenting the whole block here, as lines
    %               are expected to have their base indent already. It could be used for
    %               internal relative indenting if needed in future enhancements.
    %   indentUnit: Scalar, number of `indentChar` repetitions for one standard indent level.
    %               Similar to `indentChar`, primarily for context if deeper logic is added.
    %
    % Outputs:
    %   formattedBlockLines: Cell array of strings, representing the formatted lines
    %                        of the 'arguments' block, ready to replace the original
    %                        lines in the main beautifier output.
    %
    % Description:
    %   This function takes the raw lines from an 'arguments' block and attempts to align
    %   the argument names, size/class specifications, validation functions (in curly braces),
    %   and default values.
    %   - It parses each line to identify these components.
    %   - Full-line comments and empty lines are preserved with their original indentation.
    %   - Trailing comments on argument definition lines are preserved.
    %   - It calculates the maximum width for each component (name, size/class, validators)
    %     across all relevant lines in the block.
    %   - Lines are then reconstructed with padding to align these components vertically.
    %   - Spacing around the '=' for default values is controlled by `options.SpaceAroundOperators`.

    if isempty(blockLines)
        formattedBlockLines = {};
        return;
    end

    parsedArgs = struct('name', {}, 'sizeClass', {}, 'validators', {}, ...
                        'defaultValue', {}, 'comment', {}, 'originalLine', {}, ...
                        'isCommentOnly', {}, 'indentStr', {});
    
    % Regex patterns
    % Arg Name: captures simple names or dot-notation names (e.g., options.Value)
    namePattern = '^\s*([a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*)';
    % Size/Class: captures optional parens like (1,:) and then a type like char, string, customType
    % It is non-greedy for the content within parentheses.
    sizeClassPattern = '\s*((?:\([^\)]*?\))?\s*[a-zA-Z_]\w*)'; 
    % Validators: captures content within curly braces {}
    validatorsPattern = '\s*(\{[^\}]*\})';
    % Default Value: captures everything after an equals sign
    defaultValuePattern = '\s*=\s*(.+)'; % greedy for the rest

    for i = 1:length(blockLines)
        line = blockLines{i};
        parsedArgs(i).originalLine = line;
        parsedArgs(i).isCommentOnly = false;

        leadingWhitespace = regexp(line, '^\s*', 'match', 'once');
        parsedArgs(i).indentStr = leadingWhitespace;
        
        trimmedLine = strtrim(line);

        % Preserve empty lines as they are, just storing their original indent.
        if isempty(trimmedLine) 
            parsedArgs(i).name = ''; % Mark as not a typical arg line, ensures it's skipped in width calculation
            parsedArgs(i).comment = ''; % Effectively a blank line
            % Other fields like sizeClass, validators, defaultValue remain empty.
            continue; 
        end

        % Preserve full-line comments, storing the entire line (including its original indent via commentPart).
        if startsWith(trimmedLine, '%')
            parsedArgs(i).isCommentOnly = true;
            parsedArgs(i).comment = trimmedLine; % Store the whole line as comment
            % other fields remain empty
            continue;
        end

        % Extract comment part first
        [codePart, commentPart] = extractCodeAndCommentInternal(trimmedLine); % Use existing helper
        parsedArgs(i).comment = commentPart;
        
        currentCode = codePart; % This is the code part of the line, excluding any trailing comment.
        
        % Parsing order: Name, then Size/Class, then Validators, then Default Value.
        % This order is chosen to minimize ambiguity, as default values can be complex.

        % 1. Extract Argument Name
        nameMatch = regexp(currentCode, namePattern, 'tokens', 'once');
        if ~isempty(nameMatch)
            parsedArgs(i).name = strtrim(nameMatch{1});
            % Update currentCode to exclude the matched name part for subsequent parsing.
            currentCode = currentCode(length(nameMatch{1})+1:end); 
        else
            parsedArgs(i).name = ''; % Should ideally not happen for a valid argument line.
        end
        currentCode = strtrim(currentCode); % Trim for next parsing step.

        % 2. Extract Size and Class Specification (e.g., (1,:) char, string)
        % This pattern is anchored to the beginning of the remaining currentCode.
        sizeClassMatch = regexp(currentCode, ['^', sizeClassPattern], 'tokens', 'once');
        if ~isempty(sizeClassMatch)
            parsedArgs(i).sizeClass = strtrim(sizeClassMatch{1});
            % Update currentCode: remove the matched size/class part.
            % regexprep is used for safe removal based on the matched string.
            currentCode = regexprep(currentCode, ['^', regexptranslate('escape', parsedArgs(i).sizeClass)], '', 'once');
        else
            parsedArgs(i).sizeClass = '';
        end
        currentCode = strtrim(currentCode);
        
        % 3. Extract Validation Functions (e.g., {mustBeNumeric, mustBePositive})
        % Anchored to the beginning of the remaining currentCode.
        validatorsMatch = regexp(currentCode, ['^', validatorsPattern], 'tokens', 'once');
         if ~isempty(validatorsMatch)
            parsedArgs(i).validators = strtrim(validatorsMatch{1});
            % Update currentCode: remove the matched validators part.
            currentCode = regexprep(currentCode, ['^', regexptranslate('escape', parsedArgs(i).validators)], '', 'once');
        else
            parsedArgs(i).validators = '';
        end
        currentCode = strtrim(currentCode);

        % 4. Extract Default Value (e.g., = "default", = 10)
        % This should be what's left, starting with an '='.
        if startsWith(currentCode, '=') % Check if the remainder starts with '='
            defaultMatch = regexp(currentCode, defaultValuePattern, 'tokens', 'once');
            if ~isempty(defaultMatch)
                parsedArgs(i).defaultValue = strtrim(defaultMatch{1});
            else
                 % Handle cases like "name =" (empty default)
                if strcmp(strtrim(currentCode), '=')
                    parsedArgs(i).defaultValue = ''; % Explicit empty default
                else
                    parsedArgs(i).defaultValue = ''; % Or treat as no default
                end
            end
        else
            parsedArgs(i).defaultValue = ''; % No default value part
        end
    end

    % Determine maximum widths for alignment
    maxNameLen = 0;
    maxSizeClassLen = 0;
    maxValidatorsLen = 0;

    for i = 1:length(parsedArgs)
        if parsedArgs(i).isCommentOnly || isempty(parsedArgs(i).name) % Skip comment-only or blank lines
            continue;
        end
        maxNameLen = max(maxNameLen, length(parsedArgs(i).name));
        if ~isempty(parsedArgs(i).sizeClass)
            maxSizeClassLen = max(maxSizeClassLen, length(parsedArgs(i).sizeClass));
        end
        if ~isempty(parsedArgs(i).validators)
            maxValidatorsLen = max(maxValidatorsLen, length(parsedArgs(i).validators));
        end
    end

    % Reconstruct lines
    formattedBlockLines = cell(size(blockLines));
    for i = 1:length(parsedArgs)
        if parsedArgs(i).isCommentOnly
            formattedBlockLines{i} = [parsedArgs(i).indentStr, parsedArgs(i).comment];
            continue;
        end
        % If it's an empty line (original line was only whitespace, or became so),
        % its .name will be empty, and .comment will be empty.
        if isempty(parsedArgs(i).name) && isempty(parsedArgs(i).comment) 
            formattedBlockLines{i} = parsedArgs(i).indentStr; % Preserve original indent (becomes an empty line)
            continue;
        end

        % Start reconstructing the line with its original indent.
        lineParts = {};
        lineParts{end+1} = parsedArgs(i).indentStr;
        
        % Name part
        nameStr = parsedArgs(i).name;
        namePadding = maxNameLen - length(nameStr);
        lineParts{end+1} = [nameStr, repmat(' ', 1, namePadding)];

        % Size/Class part
        if ~isempty(parsedArgs(i).sizeClass)
            sizeClassStr = parsedArgs(i).sizeClass;
            sizeClassPadding = maxSizeClassLen - length(sizeClassStr);
            lineParts{end+1} = [' ', sizeClassStr, repmat(' ', 1, sizeClassPadding)];
        elseif maxSizeClassLen > 0 % Need to pad if other lines have size/class
            lineParts{end+1} = repmat(' ', 1, maxSizeClassLen + 1); % +1 for leading space
        end

        % Validators part
        if ~isempty(parsedArgs(i).validators)
            validatorsStr = parsedArgs(i).validators;
            validatorsPadding = maxValidatorsLen - length(validatorsStr);
            lineParts{end+1} = [' ', validatorsStr, repmat(' ', 1, validatorsPadding)];
        elseif maxValidatorsLen > 0 % Need to pad if other lines have validators
             lineParts{end+1} = repmat(' ', 1, maxValidatorsLen + 1); % +1 for leading space
        end
        
        % Default value part
        if ~isempty(parsedArgs(i).defaultValue) || (~isempty(parsedArgs(i).name) && any(strcmp(parsedArgs(i).originalLine, strtrim(parsedArgs(i).name)))) % if default is explicitly set or it's just a name (like options.myOpt)
            defaultStr = parsedArgs(i).defaultValue;
            if options.SpaceAroundOperators
                lineParts{end+1} = [' = ', defaultStr];
            else
                lineParts{end+1} = ['=', defaultStr];
            end
        end
        
        fullLine = strjoin(lineParts, '');
        % Remove trailing spaces from the code part before appending a comment.
        if ~all(isspace(fullLine)) % Avoid processing if fullLine is only whitespace (e.g. just indent)
            fullLine = regexprep(fullLine, '\s+$', '');
        end

        % Append the original comment part.
        % extractCodeAndCommentInternal standardizes comments to start with ' % ...' or be '%'
        if ~isempty(parsedArgs(i).comment)
            if ~isempty(strtrim(fullLine)) % If there's actual code content before comment
                 fullLine = [fullLine, parsedArgs(i).comment]; % comment already has its leading space
            else % Line was effectively only indent, so comment directly follows indent
                 fullLine = [fullLine, strtrim(parsedArgs(i).comment)]; % Avoid double space if comment already leads with one
            end
        end
        
        formattedBlockLines{i} = fullLine; % Store the fully reconstructed line.
    end
end

% --- Helper function to align assignment blocks ---
% This function identifies blocks of consecutive assignment statements and aligns their '=' signs.
% A block is defined by consecutive lines with the same indentation level that are
% assignable statements. Empty lines or lines with different indentation break the block.
% Full-line comments with matching indents are considered part of the block but are not aligned.
function lines = alignAssignmentBlocksInternal(lines, options)
    if isempty(lines), return; end

    % State variables for tracking the current block of assignments:
    blockLinesIndices = [];      % Stores original line numbers of lines in the current block.
    blockLinesContent = {};      % Cell array of structs, each describing a line in the block 
                                 % (type: 'assignment' or 'comment', content, original index, indent string).
    blockLinesIndents = {};      % Cell array storing indent strings of actual CODE lines in the block,
                                 % used to check if subsequent lines belong to the same block.
    maxLhsLen = 0;               % Maximum length of the Left-Hand Side (LHS) of assignments in the current block.
    
    indentKeywordsPattern = ['^\s*(if|for|while|switch|try|parfor|function|classdef|properties|methods|events|arguments)\b'];

    % Helper anonymous function to reset state variables for a new block.
    resetBlockState = @() deal([], {}, {}, 0); % Returns empty for the four state vars

    for i = 1:length(lines) % Iterate through each line provided to the function
        currentLine = lines{i};
        trimmedLine = strtrim(currentLine); % Current line without leading/trailing whitespace
        currentIndent = regexp(currentLine, '^\s*', 'match', 'once'); % Get leading whitespace (indentation)

        % If the line is empty, it breaks any current assignment block.
        if isempty(trimmedLine)
            if ~isempty(blockLinesIndices) % If a block was being formed
                lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options); % Apply alignment to it
            end
            [blockLinesIndices, blockLinesContent, blockLinesIndents, maxLhsLen] = resetBlockState(); % Reset state
            continue; % Move to the next line
        end

        isFullCommentLine = startsWith(trimmedLine, '%'); % Check if the line is a full-line comment
        isAssignable = false; % Flag to indicate if the current line is an alignable assignment
        lhs = ''; rhs = ''; commentPartForAssignment = ''; equalsIndexInCode = -1; % Variables for assignment parts

        % If not a full comment line, try to parse as code.
        if ~isFullCommentLine
            [codePart, commentPartExtracted] = extractCodeAndCommentInternal(trimmedLine); % Separate code and trailing comment
            isKeywordLine = ~isempty(regexp(codePart, indentKeywordsPattern, 'once')); % Check if line starts with a major keyword

            % Consider for assignment alignment if not a keyword line and not ending with line continuation '...'
            if ~isKeywordLine && ~endsWith(strtrim(codePart), '...')
                % This section finds the first '=' that is a standalone assignment operator,
                % not part of '==', '>=', etc., and not within a string literal.
                tempCodeForEquals = codePart;
                inSingleQuote = false; inDoubleQuote = false; % Flags for being inside string literals
                tempEqualsIndex = -1; % Index of the found assignment '='
                for charIdx = 1:length(tempCodeForEquals) % Iterate through characters in the code part
                    char = tempCodeForEquals(charIdx);
                    if char == '''' % Single quote
                        if charIdx+1 <= length(tempCodeForEquals) && tempCodeForEquals(charIdx+1) == '''' % Escaped single quote ('')
                            charIdx = charIdx + 1; % Skip next quote character
                        elseif ~inDoubleQuote % Only toggle if not inside a double-quoted string
                            inSingleQuote = ~inSingleQuote;
                        end
                    elseif char == '"' % Double quote
                         if charIdx+1 <= length(tempCodeForEquals) && tempCodeForEquals(charIdx+1) == '"' % Escaped double quote ("")
                            charIdx = charIdx + 1; % Skip next quote character
                         elseif ~inSingleQuote % Only toggle if not inside a single-quoted string
                            inDoubleQuote = ~inDoubleQuote;
                         end
                    elseif char == '=' && ~inSingleQuote && ~inDoubleQuote % Found an '=' not in a string
                        % Check if it's a standalone assignment '=' and not part of '==', '>=', etc.
                        isComparison = false;
                        if charIdx > 1 && ismember(tempCodeForEquals(charIdx-1), {'=', '~', '<', '>'}) % Check char before
                            isComparison = true;
                        end
                        if charIdx < length(tempCodeForEquals) && tempCodeForEquals(charIdx+1) == '=' % Check char after
                            isComparison = true;
                        end
                        if ~isComparison
                            tempEqualsIndex = charIdx; % Store index of valid assignment '='
                            break; 
                        end
                    end
                end
                equalsIndexInCode = tempEqualsIndex;

                if equalsIndexInCode > 0 % If a valid assignment '=' was found
                    isAssignable = true;
                    lhs = strtrim(codePart(1:equalsIndexInCode-1)); % Extract Left-Hand Side
                    rhs = strtrim(codePart(equalsIndexInCode+1:end)); % Extract Right-Hand Side
                    commentPartForAssignment = commentPartExtracted; % Store its trailing comment
                end
            end
        end

        % Decision Logic: Based on whether the line is assignable, a full comment, or other code.
        if isAssignable
            % If it's the first line in a potential block OR its indent matches the last CODE line's indent in the current block:
            if isempty(blockLinesIndices) || strcmp(currentIndent, blockLinesIndents{end})
                blockLinesIndices(end+1) = i; % Add line index to block
                blockLinesContent{end+1} = struct('type', 'assignment', ... % Store details
                                                  'lhs', lhs, 'rhs', rhs, ...
                                                  'comment', commentPartForAssignment, ...
                                                  'originalIndex', i, 'indentStr', currentIndent);
                blockLinesIndents{end+1} = currentIndent; % Record indent of this CODE line for the block
                % Update maximum LHS length for alignment calculation
                if options.UseTabs % Tab handling for LHS length is approximate
                    maxLhsLen = max(maxLhsLen, length(lhs));
                else
                    maxLhsLen = max(maxLhsLen, length(lhs));
                end
            else % Assignable line, but its indent differs from the current block's code lines.
                if ~isempty(blockLinesIndices) % Process the previously accumulated block.
                    lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options);
                end
                [blockLinesIndices, blockLinesContent, blockLinesIndents, maxLhsLen] = resetBlockState(); % Reset for a new block.
                % Start a new block with the current assignable line.
                blockLinesIndices(end+1) = i;
                blockLinesContent{end+1} = struct('type', 'assignment', ...
                                                  'lhs', lhs, 'rhs', rhs, ...
                                                  'comment', commentPartForAssignment, ...
                                                  'originalIndex', i, 'indentStr', currentIndent);
                blockLinesIndents{end+1} = currentIndent;
                if options.UseTabs
                    maxLhsLen = max(maxLhsLen, length(lhs));
                else
                    maxLhsLen = max(maxLhsLen, length(lhs));
                end
            end
        elseif isFullCommentLine
            % If the line is a full comment:
            % And a block is active AND the comment's indent matches the block's code line indent:
            if ~isempty(blockLinesIndices) && strcmp(currentIndent, blockLinesIndents{end})
                % Add comment to the current block as a 'comment' type. It doesn't break the block.
                % These comments are preserved but not used for `maxLhsLen` calculation.
                blockLinesIndices(end+1) = i;
                blockLinesContent{end+1} = struct('type', 'comment', ...
                                                  'lineValue', lines{i}, ... % Store original full line content
                                                  'originalIndex', i, 'indentStr', currentIndent);
                % Note: Comment lines do not contribute to `blockLinesIndents` or `maxLhsLen`.
            else % Comment line either has a different indent or there's no active block.
                if ~isempty(blockLinesIndices) % Process any existing block.
                    lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options);
                end
                [blockLinesIndices, blockLinesContent, blockLinesIndents, maxLhsLen] = resetBlockState(); % Reset.
                % The comment line itself is preserved in `lines{i}` from the main loop's processing.
            end
        else % Line is not assignable AND not a full comment (e.g., 'if', 'for', other statements).
            if ~isempty(blockLinesIndices) % This line breaks any current block. Process it.
                lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options);
            end
            [blockLinesIndices, blockLinesContent, blockLinesIndents, maxLhsLen] = resetBlockState(); % Reset.
            % The current line `lines{i}` is left as is (its formatting handled by main loop).
        end
    end

    % After the loop, process any remaining block that was not yet finalized.
    if ~isempty(blockLinesIndices)
        lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options);
    end
end

% This function applies the actual alignment to a collected block of lines (assignments and comments).
function lines = applyAlignmentToBlock(lines, blockContent, maxLhsLen, options)
    if length(blockContent) < 1, return; end % Do nothing for empty blocks.
    
    % Optional: Could add a check here to only align if numAssignmentsInBlock >= 2, for example.
    % numAssignments = 0;
    % for k_chk = 1:length(blockContent)
    %     if strcmp(blockContent{k_chk}.type, 'assignment')
    %         numAssignments = numAssignments + 1;
    %     end
    % end
    % if numAssignments < 1 % Or < 2 for stricter alignment. For now, allows single line "block" to be formatted.
    %     return; 
    % end

    for k = 1:length(blockContent) % Iterate through each item (assignment or comment) in the block
        item = blockContent{k};
        idx = item.originalIndex; % Original line number
        
        if strcmp(item.type, 'assignment') % Only format assignment lines
            blockIndent = item.indentStr; % Use the indent stored with this specific assignment line
            
            % Calculate number of spaces needed to align this line's '=' with `maxLhsLen`.
            numSpacesBeforeEquals = maxLhsLen - length(item.lhs);
            spacesBeforeEqualsStr = repmat(' ', 1, numSpacesBeforeEquals); % String of spaces
            
            % Construct the new aligned line.
            if options.SpaceAroundOperators % Control spacing around '='
                newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, ' = ', item.rhs, item.comment];
            else
                newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, '=', item.rhs, item.comment];
            end
            lines{idx} = regexprep(newLine, '\s+$', ''); % Update the line in the main `lines` cell array, trim trailing whitespace.
        elseif strcmp(item.type, 'comment')
            % Comment lines within the block are preserved as they are.
            % Their original content (including their own indentation, which should match the block's code)
            % is already in `lines{idx}` from the main processing loop or from `item.lineValue` if stored.
            % No reformatting action is needed for 'comment' type items here.
        end
    end
end


% --- Helper function to extract code and comment parts ---
% This helper robustly separates the code part of a line from its trailing comment.
% It correctly handles '%' characters that might appear inside single or double quoted string literals
% by tracking whether the parser is currently inside such a literal using `inSingleQuoteString`
% and `inDoubleQuoteString` flags.
function [codeP, commentP] = extractCodeAndCommentInternal(lineStr)
    trimmedLine = strtrim(lineStr); % Remove leading/trailing whitespace from the raw line
    codeP = trimmedLine; % Initialize code part as the whole trimmed line
    commentP = ''; % Initialize comment part as empty
    
    len = length(trimmedLine);
    actualCommentStartIdx = -1; % Will store the index where the actual comment symbol '%' is found
    
    % Flags to track if currently inside a string literal
    inSingleQuoteString = false; 
    inDoubleQuoteString = false;
    
    i = 1;
    while i <= len % Iterate through each character of the trimmed line
        char = trimmedLine(i);
        
        if char == '''' % Encountered a single quote
            if ~inDoubleQuoteString % Only process if not currently inside a double-quoted string
                if i+1 <= len && trimmedLine(i+1) == '''' % Check for an escaped single quote (e.g., 'it''s')
                    i = i + 1; % Skip the next quote, as it's part of the literal
                else
                    inSingleQuoteString = ~inSingleQuoteString; % Toggle state: enter/exit single-quoted string
                end
            end
        elseif char == '"' % Encountered a double quote
            if ~inSingleQuoteString % Only process if not currently inside a single-quoted string
                if i+1 <= len && trimmedLine(i+1) == '"' % Check for an escaped double quote (e.g., "say ""hello""") (less common in MATLAB for this)
                    i = i + 1; % Skip the next quote
                else
                    inDoubleQuoteString = ~inDoubleQuoteString; % Toggle state: enter/exit double-quoted string
                end
            end
        elseif char == '%' % Encountered a percent sign
            if ~inSingleQuoteString && ~inDoubleQuoteString % If NOT inside any string literal
                actualCommentStartIdx = i; % This is the start of the actual comment
                break; % Stop searching, comment found
            end
            % If inside a string, this '%' is part of the string, not a comment starter.
        end
        i = i + 1;
    end

    if actualCommentStartIdx ~= -1 % If a valid comment starting '%' was found
        if actualCommentStartIdx == 1 % If the comment starts at the beginning of the trimmed line
            codeP = ''; % No code part
            commentP = trimmedLine; % The entire trimmed line is the comment
        else
            % The code part is everything before the comment's '%'
            codeP = strtrim(trimmedLine(1:actualCommentStartIdx-1));
            % The comment part starts from '%' and includes its content.
            % Standardize to have a space after '%' if there's content.
            commentContent = strtrim(trimmedLine(actualCommentStartIdx+1:end));
            if isempty(commentContent) && actualCommentStartIdx == len % Trailing '%' like "code %"
                 commentP = '%'; % Just the '%' symbol
            elseif isempty(commentContent) % Trailing '%' with space like "code % "
                 commentP = ' %'; % Standardize to '% '
            else
                commentP = [' % ', commentContent]; % Standardize to " % content"
            end
        end
    % else: no comment found (or '%' was only inside strings). 
    % `codeP` remains the whole `trimmedLine`, `commentP` remains empty.
    end
end

% --- Validation function for StylePreset ---
% Validates the 'StylePreset' option name.
function validateStylePreset(presetName)
    % Check type: must be char or scalar string.
    if ~(ischar(presetName) || (isstring(presetName) && isscalar(presetName)))
        ME = MException('code_beautifier:InvalidStylePresetType', ...
                        'StylePreset must be a character vector or a scalar string.');
        throwAsCaller(ME); % Throw error as if from the main function for better user context.
    end
    presetName = char(presetName); % Convert to char for consistent comparison.

    if isempty(presetName) % An empty preset name means no preset is applied (use defaults).
        return; 
    end
    
    % Define valid preset names (case-insensitive check, but canonical names are used internally).
    validPresetNames = {'Default', 'MathWorksStyle', 'CompactStyle'}; 
    isActuallyValid = false;
    for i = 1:length(validPresetNames)
        if strcmpi(presetName, validPresetNames{i}) % Case-insensitive comparison
            isActuallyValid = true;
            break;
        end
    end

    if ~isActuallyValid % If the provided name is not in the list of valid presets.
        ME = MException('code_beautifier:InvalidStylePreset', ...
                        'Unknown StylePreset: "%s". Valid presets are ''Default'', ''MathWorksStyle'', ''CompactStyle''. Use an empty string for no preset.', presetName);
        throwAsCaller(ME);
    end
end

% --- Helper function to get known option types and validators ---
% This function returns a struct containing information about known options,
% primarily used for parsing the .mbeautifyrc config file.
function knownInfo = getKnownOptionsInfo(defaultSettings)
    knownInfo = struct(); % Initialize struct to store info about each known option
    optionNames = fieldnames(defaultSettings); % Get names of all default options
    for i = 1:length(optionNames)
        optName = optionNames{i};
        value = defaultSettings.(optName); % Get default value to infer type
        % Determine type and specific validators if needed
        if islogical(value)
            knownInfo.(optName).type = 'logical';
        elseif isnumeric(value)
            knownInfo.(optName).type = 'numeric';
            % Add specific validators for numeric options with restricted ranges/types
            if strcmp(optName, 'MinBlankLinesBeforeBlock')
                knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x) && x >= 0 && x <= 2 && floor(x) == x;
                knownInfo.(optName).range = [0, 2]; % For constructing error messages
            elseif any(strcmp(optName, {'IndentSize', 'ContinuationIndentOffset'}))
                knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x; % Must be non-negative integer
            else 
                knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x); % Basic numeric check
            end
        elseif ischar(value) || isstring(value) % String options
            knownInfo.(optName).type = 'string';
            if strcmp(optName, 'OutputFormat')
                knownInfo.(optName).validator = @(x) (ischar(x) || (isstring(x) && isscalar(x))) && ismember(lower(char(x)), {'cell', 'char'});
                knownInfo.(optName).allowed = {'cell', 'char'}; % For error messages
            elseif strcmp(optName, 'StylePreset') % StylePreset option itself can be in config
                knownInfo.(optName).validator = @validateStylePresetConfig; % Use a slightly different validator for config context
            end
        end
    end
end

% Validation for 'StylePreset' when read from a config file.
% Differs from `validateStylePreset` as it throws errors directly for `parseConfigFile` to catch.
function validateStylePresetConfig(presetName)
    if ~(ischar(presetName) || (isstring(presetName) && isscalar(presetName)))
        ME = MException('code_beautifier:InvalidStylePresetTypeInConfigFile', ...
                        'StylePreset in config file must be a string.');
        throw(ME); % Throw directly; parseConfigFile will catch and issue a warning.
    end
    presetNameStr = char(presetName);
    if isempty(presetNameStr) % Empty is allowed (means no preset from config)
        return; 
    end
    validPresets = {'Default', 'MathWorksStyle', 'CompactStyle'}; % Valid preset names
    if ~ismember(lower(presetNameStr), lower(validPresets)) % Case-insensitive check
        ME = MException('code_beautifier:InvalidStylePresetInConfigFile', ...
                        'Unknown StylePreset "%s" in config file. Valid are ''Default'', ''MathWorksStyle'', ''CompactStyle''.', presetNameStr);
        throw(ME);
    end
end


% --- Helper function to parse .mbeautifyrc config file ---
% Parses the .mbeautifyrc file for beautifier options.
function parsedOptions = parseConfigFile(filePath, knownInfo)
    parsedOptions = struct(); % Initialize struct to hold options found in the file.
    try
        fid = fopen(filePath, 'rt'); % Open file in text read mode.
        if fid == -1 % If file cannot be opened (e.g., doesn't exist, permissions).
            warning('code_beautifier:ConfigFileNotFound', ...
                    'Configuration file .mbeautifyrc not found or cannot be opened.');
            return;
        end
        C = onCleanup(@() fclose(fid)); % Ensure file is closed when function exits or errors.
        
        lineNumber = 0;
        while ~feof(fid) % Read file line by line.
            lineNumber = lineNumber + 1;
            line = strtrim(fgetl(fid)); % Get line and remove leading/trailing whitespace.
            
            % Skip empty lines and lines starting with '#' (comments).
            if isempty(line) || startsWith(line, '#')
                continue;
            end
            
            % Parse "key = value" pairs. Regex captures key and value.
            % It allows for spaces around '=' and trims spaces from key/value.
            % It also ignores anything after a '#' on the value side (trailing comment).
            parts = regexp(line, '^\s*([^#=\s]+)\s*=\s*([^#]+?)\s*$', 'tokens');
            if isempty(parts) % If line doesn't match "key = value" format.
                warning('code_beautifier:InvalidLineInConfigFile', ...
                        'Skipping invalid line %d in .mbeautifyrc: "%s". Line must be in "key = value" format.', lineNumber, line);
                continue;
            end
            
            key = strtrim(parts{1}{1}); % Extracted key.
            valueStr = strtrim(parts{1}{2}); % Extracted value as a string.
            
            % Find the canonical option name (case-insensitive matching against known option names).
            % `canonicalKey` will be the version of the key as defined in `stylePresets` (e.g., 'IndentSize').
            canonicalKey = '';
            knownOptionNames = fieldnames(knownInfo);
            for k_idx = 1:length(knownOptionNames)
                if strcmpi(key, knownOptionNames{k_idx}) % Case-insensitive comparison
                    canonicalKey = knownOptionNames{k_idx};
                    break;
                end
            end
            
            if isempty(canonicalKey) % If the key is not a known option.
                warning('code_beautifier:UnknownConfigFileOption', ...
                        'Skipping unknown option "%s" on line %d in .mbeautifyrc.', key, lineNumber);
                continue;
            end
            
            % Convert the string value (`valueStr`) to its appropriate data type based on `knownInfo`.
            info = knownInfo.(canonicalKey); % Get type and validator info for this option.
            try
                parsedValue = [];
                switch info.type % Determine type for conversion
                    case 'logical'
                        if strcmpi(valueStr, 'true')
                            parsedValue = true;
                        elseif strcmpi(valueStr, 'false')
                            parsedValue = false;
                        else
                            error('Value must be "true" or "false" (case-insensitive).'); % Error for invalid logical string
                        end
                    case 'numeric'
                        parsedValue = str2double(valueStr); % Convert string to double
                        if isnan(parsedValue) % Check if conversion failed
                            error('Invalid numeric value: "%s".', valueStr);
                        end
                        % Validate numeric value against specific constraints if a validator exists
                        if isfield(info, 'validator') && ~info.validator(parsedValue)
                            if isfield(info, 'range') % Provide range in error if available
                                error('Numeric value %g is out of allowed range [%g, %g] or not a valid integer.', parsedValue, info.range(1), info.range(2));
                            else
                                error('Numeric value %g is not valid for option "%s".', parsedValue, canonicalKey);
                            end
                        end
                    case 'string'
                        parsedValue = valueStr; % Value is already a string.
                        % Validate string value if a specific validator or allowed list exists.
                        if strcmp(canonicalKey, 'StylePreset') % Special handling for StylePreset string in config
                           validateStylePresetConfig(parsedValue); % Use the config-specific validator
                           % Ensure canonical casing for preset names if matched from config
                            validPresetNamesInner = {'Default', 'MathWorksStyle', 'CompactStyle'};
                            for vpi = 1:length(validPresetNamesInner)
                                if strcmpi(parsedValue, validPresetNamesInner{vpi})
                                    parsedValue = validPresetNamesInner{vpi}; % Use canonical casing
                                    break;
                                end
                            end
                        elseif isfield(info, 'validator') && ~info.validator(parsedValue)
                             if isfield(info, 'allowed') % Provide allowed values in error if available
                                error('String value "%s" is not one of the allowed values: %s.', parsedValue, strjoin(info.allowed, ', '));
                            else
                                error('String value "%s" is not valid for option "%s".', parsedValue, canonicalKey);
                            end
                        end
                    otherwise % Should not happen if knownInfo is correctly populated.
                        warning('code_beautifier:InternalParserError', ...
                                'Internal error: Unknown type "%s" for option "%s". Skipping.', info.type, canonicalKey);
                        continue;
                end
                parsedOptions.(canonicalKey) = parsedValue; % Store the parsed and validated option.
            catch ME % Catch errors during parsing/validation of a single option's value.
                warning('code_beautifier:InvalidValueInConfigFile', ...
                        'Skipping option "%s" on line %d in .mbeautifyrc due to invalid value: %s (%s)', ...
                        canonicalKey, lineNumber, valueStr, ME.message);
            end
        end
    catch ME_file % Catch errors related to file operations (e.g., opening the file).
        warning('code_beautifier:ErrorReadingConfigFile', 'Error reading .mbeautifyrc: %s', ME_file.message);
    end
