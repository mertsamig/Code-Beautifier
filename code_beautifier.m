function beautifulCode = code_beautifier(varargin)
% code_beautifier Formats the MATLAB code in the active editor.
%
%   beautifulCode = code_beautifier()
%   Formats the MATLAB code currently active in the MATLAB editor.
%   The function retrieves the code automatically from the active editor window.
%   Formatting uses default settings unless overridden by Name-Value pairs.
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
%   'OutputFormat':             String, 'char' (default) or 'cell'. Defines output type.
%
% Example (Basic Usage):
%   % To format the currently active script in the MATLAB editor using default settings:
%   formattedCode = code_beautifier();
%   % To display the formatted code in the command window:
%   disp(formattedCode);
%
% Example (With Options):
%   % Format the active script using the 'MathWorksStyle' preset and specific 'IndentSize':
%   formattedCodeWithOptions = code_beautifier('StylePreset', 'MathWorksStyle', 'IndentSize', 2);
%   disp(formattedCodeWithOptions);
%
% Example (Replacing Editor Content - Use with Caution):
%   % To replace the content of the active editor with the formatted code:
%   % beautifulOutput = code_beautifier('OutputFormat', 'char'); % Ensure output is char
%   % activeDoc = matlab.desktop.editor.getActive();
%   % if ~isempty(activeDoc) && ~isempty(beautifulOutput)
%   %   activeDoc.Text = beautifulOutput;
%   %   disp('Active editor content has been updated with formatted code.');
%   % else
%   %   disp('No active editor found or formatted code is empty; editor not updated.');
%   % end
%   % Note: Modifying editor content directly should be done with caution. Consider
%   %       making a backup or using version control before running such commands.

% --- Get Code from Active Editor ---
% This section attempts to get the code from the currently active MATLAB editor window.
% This is now the only source of code for the beautifier.
rawCode = ''; % Initialize rawCode
try
    editorDoc = matlab.desktop.editor.getActive();
    if isempty(editorDoc)
        error('code_beautifier:NoActiveEditor', 'No active editor document found. Please open a script or select an editor window.');
    end
    activeCodeText = editorDoc.Text;
    if isempty(strtrim(activeCodeText))
        error('code_beautifier:ActiveEditorEmpty', 'The active editor document is empty or contains only whitespace.');
    end
    rawCode = activeCodeText; % Assign active editor text to rawCode
catch ME
    % Rethrow caught errors. This handles cases where matlab.desktop.editor might not be available
    % or other unexpected issues during editor interaction.
    rethrow(ME);
end

% --- Style Presets Definition ---
stylePresets = struct();
stylePresets.Default = struct(...
    'StylePreset', 'Default', ... % Default preset name
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
    'FormatArgumentsBlock', false, ...
    'OutputFormat', 'char' ... % Set default OutputFormat to 'char'
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

% --- Determine Effective Defaults ---
% New precedence:
% 1. Hardcoded Defaults (stylePresets.Default)
% 2. Config File Preset (if specified and valid)
% 3. Config File Individual Settings (override config file's preset)
% 4. Direct Argument Preset (overrides all config file settings)
% 5. Direct Argument Individual Settings (handled by inputParser, override everything)

% 1. Start with hardcoded function defaults
effectiveDefaults = stylePresets.Default;

% 2. Parse the config file
knownOptionsInfo = getKnownOptionsInfo(stylePresets.Default);
configFilePath = fullfile(pwd, '.mbeautifyrc');
configFileOptions = struct();
if exist(configFilePath, 'file')
    configFileOptions = parseConfigFile(configFilePath, knownOptionsInfo);
end

% 3. Apply preset from config file, if any
configDefinedPresetName = ''; % Stores the canonical name of a valid preset from config
if isfield(configFileOptions, 'ConfigFileStylePreset') && ~isempty(configFileOptions.ConfigFileStylePreset)
    try
        validateStylePreset(configFileOptions.ConfigFileStylePreset); % Validate name (throws error if invalid type)
        tempConfigPresetName = char(configFileOptions.ConfigFileStylePreset); % Store the name from config

        % Canonicalize tempConfigPresetName
        validPresetNamesForConfig = fieldnames(stylePresets);
        isKnownConfigPreset = false;
        canonicalConfigPresetName = '';
        for vp_idx = 1:length(validPresetNamesForConfig)
            if strcmpi(tempConfigPresetName, validPresetNamesForConfig{vp_idx})
                canonicalConfigPresetName = validPresetNamesForConfig{vp_idx}; % Store canonical if known
                isKnownConfigPreset = true;
                break;
            end
        end

        if isKnownConfigPreset % Apply settings if it's a known preset
            configDefinedPresetName = canonicalConfigPresetName; % Update with the canonical name
            presetSettingsToApply = stylePresets.(configDefinedPresetName);
            fieldsToUpdate = fieldnames(presetSettingsToApply);
            for k_f = 1:length(fieldsToUpdate)
                fieldName = fieldsToUpdate{k_f};
                effectiveDefaults.(fieldName) = presetSettingsToApply.(fieldName);
            end
            % effectiveDefaults.StylePreset is now configDefinedPresetName
        else
            % If not a known preset (e.g., 'MyCustomPreset' not in stylePresets struct), issue warning.
            % validateStylePreset already checks against a list of known names and would error.
            % This 'else' might be redundant if validateStylePreset is strict enough.
            % However, keeping it for robustness against changes in validateStylePreset.
            warning('code_beautifier:UnknownStylePresetInConfigFile', ...
                'Unknown StylePreset "%s" in .mbeautifyrc. This preset will not be applied. Using defaults from "%s" or prior settings.', ...
                configFileOptions.ConfigFileStylePreset, effectiveDefaults.StylePreset);
        end
    catch ME_config_preset
        % If validateStylePreset threw an error (e.g. invalid type or unknown name based on its internal list).
        warning('code_beautifier:InvalidStylePresetInConfigFile', ...
            'Invalid StylePreset "%s" in .mbeautifyrc: %s. Ignoring this preset. Using defaults from "%s" or prior settings.', ...
            configFileOptions.ConfigFileStylePreset, ME_config_preset.message, effectiveDefaults.StylePreset);
    end
end

% 4. Overlay individual settings from config file (these override the config file's preset if one was applied)
fieldsToUpdate = fieldnames(configFileOptions);
for k_f = 1:length(fieldsToUpdate)
    fieldName = fieldsToUpdate{k_f};
    if strcmpi(fieldName, 'ConfigFileStylePreset')
        continue; % Skip this special field, already handled
    end
    if isfield(effectiveDefaults, fieldName) % Check if it's a known option
        effectiveDefaults.(fieldName) = configFileOptions.(fieldName);
    else
        % This case should ideally not be reached if parseConfigFile only returns known options
        warning('code_beautifier:UnknownOptionInConfigFileProcessing', ...
                'Ignoring unknown option "%s" from config file during effective defaults determination.', fieldName);
    end
end

% 5. Determine and apply preset from direct arguments (this overrides everything from config)
directArgPresetNameInput = ''; % The name as provided in varargin
for k_v = 1:2:length(varargin)
    if strcmpi(varargin{k_v}, 'StylePreset') && k_v + 1 <= length(varargin)
        directArgPresetNameInput = char(varargin{k_v+1});
        break;
    end
end

if ~isempty(directArgPresetNameInput) % If a 'StylePreset' was passed in varargin
    canonicalDirectArgPresetName = ''; % Store canonical name if valid and known
    try
        validateStylePreset(directArgPresetNameInput); % Validate name structure/type and against known names
        
        % Canonicalize directArgPresetNameInput
        validPresetNamesForDirect = fieldnames(stylePresets);
        isKnownDirectPreset = false;
        for vp_idx = 1:length(validPresetNamesForDirect)
            if strcmpi(directArgPresetNameInput, validPresetNamesForDirect{vp_idx})
                canonicalDirectArgPresetName = validPresetNamesForDirect{vp_idx}; % Use canonical casing
                isKnownDirectPreset = true;
                break;
            end
        end

        if isKnownDirectPreset
            % Apply settings from the known, valid direct argument preset
            presetSettingsToApply = stylePresets.(canonicalDirectArgPresetName);
            fieldsToUpdate = fieldnames(presetSettingsToApply);
            for k_f = 1:length(fieldsToUpdate)
                fieldName = fieldsToUpdate{k_f};
                effectiveDefaults.(fieldName) = presetSettingsToApply.(fieldName);
            end
            % effectiveDefaults.StylePreset is now canonicalDirectArgPresetName
        else
            % An unknown (but structurally valid by validateStylePreset's initial checks)
            % preset name was provided. validateStylePreset should ideally catch unknown names.
            % If it reaches here, it means validateStylePreset might not be strict enough on names.
            % Set effectiveDefaults.StylePreset to this non-canonical/unknown name;
            % inputParser's validation for 'StylePreset' (which also calls validateStylePreset)
            % will be the final gate.
            effectiveDefaults.StylePreset = directArgPresetNameInput; 
            % No warning here, as inputParser will handle it.
        end
    catch ME_direct_preset
        % An invalid (e.g., not a string, or fails validateStylePreset's checks) preset name was given.
        % Set effectiveDefaults.StylePreset to this problematic name; inputParser will catch it.
        effectiveDefaults.StylePreset = directArgPresetNameInput;
        % No warning here, as inputParser will handle it.
    end
% If directArgPresetNameInput is empty, effectiveDefaults.StylePreset already holds
% the preset name determined from config (if any, and valid) or the initial default.
% This value is what's passed to inputParser's 'StylePreset' parameter default.
end

% --- Input Parsing ---
% Initialize options with effectiveDefaults.
options = effectiveDefaults;

if isempty(varargin)
    % No direct arguments provided, so options are solely based on effectiveDefaults.
    % Manually add rawCode to the options struct.
    options.rawCode = rawCode;
else
    % Direct arguments were provided, use inputParser to parse them.
    p = inputParser;
    % Add rawCode as a required argument.
    addRequired(p, 'rawCode', @(x) (ischar(x) || isstring(x) || iscellstr(x)));

    % Add all beautifier options as parameters.
    % Default values are taken from the 'options' struct, which currently holds effectiveDefaults.
    addParameter(p, 'StylePreset', options.StylePreset, @validateStylePreset);
    addParameter(p, 'IndentSize', options.IndentSize, @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x);
    addParameter(p, 'UseTabs', options.UseTabs, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'SpaceAroundOperators', options.SpaceAroundOperators, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'SpaceAfterComma', options.SpaceAfterComma, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'ContinuationIndentOffset', options.ContinuationIndentOffset, @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x);
    addParameter(p, 'PreserveBlankLines', options.PreserveBlankLines, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'MinBlankLinesBeforeBlock', options.MinBlankLinesBeforeBlock, @(x) isnumeric(x) && isscalar(x) && x >= 0 && x <=2 && floor(x) == x);
    addParameter(p, 'RemoveRedundantSemicolons', options.RemoveRedundantSemicolons, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'AddSemicolonsToStatements', options.AddSemicolonsToStatements, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'AlignAssignments', options.AlignAssignments, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'FormatArgumentsBlock', options.FormatArgumentsBlock, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'OutputFormat', options.OutputFormat, @(x) (ischar(x) || (isstring(x) && isscalar(x))) && ismember(lower(char(x)), {'cell', 'char'}));

    % Parse varargin against the defined parameters.
    % rawCode is passed as the required argument.
    % Name-Value pairs in varargin will override defaults set from 'options' struct.
    parse(p, rawCode, varargin{:});
    options = p.Results; % Update options with the parsed results.
end

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

% --- DEBUG: Print Final Options ---
fprintf('DEBUG: options.StylePreset = %s\n', char(options.StylePreset)); % Ensure char for string
fprintf('DEBUG: options.IndentSize = %d\n', options.IndentSize);
fprintf('DEBUG: options.UseTabs = %d\n', options.UseTabs);
fprintf('DEBUG: options.SpaceAroundOperators = %d\n', options.SpaceAroundOperators);
fprintf('DEBUG: options.SpaceAfterComma = %d\n', options.SpaceAfterComma);
fprintf('DEBUG: options.ContinuationIndentOffset = %d\n', options.ContinuationIndentOffset);
fprintf('DEBUG: options.PreserveBlankLines = %d\n', options.PreserveBlankLines);
fprintf('DEBUG: options.MinBlankLinesBeforeBlock = %d\n', options.MinBlankLinesBeforeBlock);
fprintf('DEBUG: options.RemoveRedundantSemicolons = %d\n', options.RemoveRedundantSemicolons);
fprintf('DEBUG: options.AddSemicolonsToStatements = %d\n', options.AddSemicolonsToStatements);
fprintf('DEBUG: options.AlignAssignments = %d\n', options.AlignAssignments);
fprintf('DEBUG: options.FormatArgumentsBlock = %d\n', options.FormatArgumentsBlock);
fprintf('DEBUG: options.OutputFormat = %s\n', char(options.OutputFormat)); % Ensure char for string
    
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
    fprintf('DEBUG_EXTRACTION: line %d, trimmedOriginalLine="%s", codePart="%s", commentPart="%s"\n', i, trimmedOriginalLine, codePart, commentPart);

    % --- Determine First Word (Keyword) for Indentation ---
    % Check if the first word in the code part is a control keyword.
    tempWords = strsplit(strtrim(codePart)); % Split the codePart into words
    if ~isempty(tempWords)
        potentialFirstWord = tempWords{1};
        % Remove trailing semicolons or other non-alpha characters from potentialFirstWord if necessary,
        % as keywords won't have them. E.g., for "end;", potentialFirstWord might be "end;".
        % A simple way for keywords (which are usually purely alphabetic):
        potentialFirstWord = regexprep(potentialFirstWord, '[^a-zA-Z_].*$', ''); 

        if ismember(potentialFirstWord, allBlockCtrlKeywords)
            firstWord = potentialFirstWord;
        else
            firstWord = ''; % Not a control keyword
        end
    else
        firstWord = ''; % codePart was empty or only whitespace
    end
    fprintf('DEBUG_POST_FIRSTWORD_CALC: line %d, firstWord="%s", codePart="%s"\n', i, firstWord, codePart);

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
        fprintf('DEBUG_COMMENT_INDENT: line %d, currentIndentStr length=%d, level=%d, unit=%d, charVal=%d\n', i, length(currentIndentStr), currentLineEffectiveIndentLevel, indentUnit, uint8(indentChar(1)));
        tempBeautifulLines{i} = regexprep([currentIndentStr, commentPart], '\s+$', ''); % Add indent and trim trailing space
        previousLineEndedWithContinuation = false;
        previousLineActualIndentStr = currentIndentStr; % Store this comment's indent for potential next continuation
        continue; % Move to next line
    end

    % --- Stage 4: Indentation Logic for Current Code Line (that contains code) ---
    % Determine currentLineEffectiveIndentLevel & manage inCaseBody for current line processing
        if ismember(firstWord, {'case', 'otherwise'})
            % 'case' or 'otherwise' line itself is at 'indentLevel'
            currentLineEffectiveIndentLevel = indentLevel; % Set indent for the 'case'/'otherwise' line
            inCaseBody = true; % Set flag: content following this line will be in a case body
        elseif ismember(firstWord, {'elseif', 'else', 'catch'})
            currentLineEffectiveIndentLevel = max(0, indentLevel - 1); % Dedent these keywords
            inCaseBody = false; % Not in a switch-case body
        elseif ismember(firstWord, dedentKeywords) % 'end'
            currentLineEffectiveIndentLevel = max(0, indentLevel - 1); % Dedent 'end'
            % Note: Resetting inCaseBody for an 'end' that closes a switch
            % is handled in the "Update indentLevel for NEXT line" section
            % via the inSwitchBlockDepth variable.
        else % This case handles lines that are not starting with a major block control keyword
             % (e.g., firstWord is "" for a regular code line, or it's a non-keyword command)
            fprintf('DEBUG_SWITCH_CASE_CONTENT: Line i=%d, firstWord="%s", About to check inCaseBody. Value of inCaseBody: %d, indentLevel: %d\n', i, firstWord, inCaseBody, indentLevel);
            if inCaseBody % Check if we are currently inside the body of a 'case' or 'otherwise'
                currentLineEffectiveIndentLevel = indentLevel + 1; 
            else
                currentLineEffectiveIndentLevel = indentLevel; 
            end
            fprintf('DEBUG_SWITCH_CASE_CONTENT_POST: Line i=%d, firstWord="%s", currentLineEffectiveIndentLevel: %d\n', i, firstWord, currentLineEffectiveIndentLevel);
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
            fprintf('DEBUG: Entering SpaceAroundOperators block. options.SpaceAroundOperators = %d\n', options.SpaceAroundOperators);
            fprintf('DEBUG: SO: processedCodePart (before any regex): "%s"\n', processedCodePart);
            
            fprintf('DEBUG: Processing specific relational/equality ops.\n');
            relationalOps = {'==', '~=', '<=', '>=', '<', '>'}; % Longer ones first
            lhs_capture = '([\w\)\]''\.]|\d+\.\d*|\.\d+)'; % Operand before
            rhs_capture = '([\w\(\[''"]|\d+\.\d*|\.\d+)'; % Operand after
            for k_rel = 1:length(relationalOps)
                op_rel = relationalOps{k_rel};
                escaped_op_rel = regexptranslate('escape', op_rel);
                pat_rel_robust = ['(\S)\s*', escaped_op_rel, '\s*(\S)']; % Simplified pattern
                rep_rel = ['$1 ', op_rel, ' $2'];
                
                if i == 3 && strcmp(op_rel, '>') % Specific for Test 1, line 3, when processing '>'
                    fprintf('DEBUG_GT_REGEXPREP_INPUT: Line %d, Op "%s"\n', i, op_rel);
                    fprintf('    processedCodePart (input): "%s"\n', processedCodePart);
                    % Ensure pat_rel_robust used here is the one FOR THE '>' OPERATOR
                    % This means pat_rel_robust should be defined inside the loop or use escaped_op_rel
                    % The current pat_rel_robust = ['(\S)\s*', escaped_op_rel, '\s*(\S)']; is fine if escaped_op_rel is correctly '>'
                    fprintf('    pat_rel_robust (pattern for ">"): "%s"\n', pat_rel_robust); 
                    fprintf('    rep_rel (replacement for ">"): "%s"\n', rep_rel);
                    
                    temp_gt_result = regexprep(processedCodePart, pat_rel_robust, rep_rel);
                    fprintf('    temp_gt_result (output): "%s"\n', temp_gt_result);
                end
                
                old_processedCodePart_rel = processedCodePart; % For DEBUG
                processedCodePart = regexprep(processedCodePart, pat_rel_robust, rep_rel);
                if ~strcmp(old_processedCodePart_rel, processedCodePart)
                    fprintf('DEBUG_REL_OP: Line %d, Op "%s", Before: "%s", After: "%s"\n', i, op_rel, old_processedCodePart_rel, processedCodePart);
                end
            end
            fprintf('DEBUG: processedCodePart after specific relational/equality ops: "%s"\n', processedCodePart);
            
            % Keep the existing: fprintf('DEBUG: Checking for function definition = spacing. Before: "%s"\n', processedCodePart);
            
            func_def_pat_v3 = '^(\s*function\s+[^=]+?)\s*=\s*(.+)$'; % V3 pattern
            func_def_rep_v3 = '$1 = $2'; 
            
            old_processedCodePart_func_def = processedCodePart; % Use a distinct variable name
            processedCodePart = regexprep(processedCodePart, func_def_pat_v3, func_def_rep_v3, 'once');
            
            if ~strcmp(old_processedCodePart_func_def, processedCodePart)
                fprintf('DEBUG_FUNC_DEF_EQ: Line %d, After (V3 pattern): "%s"\n', i, processedCodePart);
            else
                % Check if it's actually a function definition line to avoid printing for every line
                if startsWith(strtrim(processedCodePart), 'function')
                     fprintf('DEBUG_FUNC_DEF_EQ: Line %d, No change with V3 pattern for: "%s"\n', i, old_processedCodePart_func_def);
                end
            end
            
            % General operators (excluding relational/equality, and +,- which need context, and element-wise handled by .*, ./ etc.)
            opListGeneral = { ...
                '&&', '||', ... % Logical operators (if not moved to relational, but they are often fine with \S)
                '.*', './', '.\\', '.^', ...             % Element-wise arithmetic operators
                '*', '/', '\\', '^', ...                 % Other arithmetic operators (in binary context)
                '=' ...                                  % Assignment operator (general cases)
                };
            fprintf('DEBUG: opListGeneral (updated) = {');
            for k_op_debug = 1:length(opListGeneral) % Use a different loop variable
                fprintf(' ''%s''', opListGeneral{k_op_debug});
            end
            fprintf(' }\n');
            
            % Iterate to ensure longer operators (e.g., '==') are processed before shorter ones (e.g., '=')
            % to prevent incorrect spacing.
            for op_idx = 1:length(opListGeneral)
                op = opListGeneral{op_idx};
                escaped_op = regexptranslate('escape', op); % Escape special regex characters in operator
                % Reverted pattern
                pat = ['(\S)\s*', escaped_op, '\s*(\S)'];
                rep = ['$1 ', op, ' $2']; % Replaces with: token1<space>operator<space>token2
                processedCodePart = regexprep(processedCodePart, pat, rep);
            end
            fprintf('DEBUG_SO_GENERAL: line %d, after general ops: "%s"\n', i, processedCodePart);

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
            fprintf('DEBUG_SO_PLUSMINUS: line %d, after +/- ops: "%s"\n', i, processedCodePart);

            % Unary plus/minus and scientific notation fixes (post-general spacing)
            % Fix 1: Remove space after specific preceding tokens if followed by unary +/-.
            % `unary_fix_class_1`: Characters that can precede a unary operator (e.g., '(', '[', '=', etc.).
            % `pat_unary_fix_1`: Matches <preceding_char><one_or_more_spaces><+/-><operand_char>.
            % Result: <preceding_char><+/-><operand_char> (removes the unwanted space).
            % Removed '=' from unary_fix_class_1 to prevent removing space after assignment, e.g. x = -5.
            unary_fix_class_1 = '[\(\[\{,\s&|]'; 
            pat_unary_fix_1 = ['(', unary_fix_class_1, ')\s+([+\-])\s*(\w|[\.\(])'];
            processedCodePart = regexprep(processedCodePart, pat_unary_fix_1, '$1$2$3');
            fprintf('DEBUG_SO_UNARY1: line %d, after unary_fix_1: "%s"\n', i, processedCodePart);

            % Fix 2: Remove space if unary +/- is at the beginning of the code part (after indent).
            % `pat_unary_fix_2`: Matches <start_of_code><+/-><one_or_more_spaces><operand_char>.
            pat_unary_fix_2 = ['^([+\-])\s+(\w|[\.\(])'];
            processedCodePart = regexprep(processedCodePart, pat_unary_fix_2, '$1$2');
            fprintf('DEBUG_SO_UNARY2: line %d, after unary_fix_2: "%s"\n', i, processedCodePart);

            % Correct spacing for lists of numbers like [1 -2 +3] or matrix rows [1 -2; 3 4]
            % This pattern identifies a digit, followed by spaces, a +/- operator, more spaces, and another digit.
            % It changes "N op N" (e.g., "1 - 2") to "N opN" (e.g., "1 -2").
            % This is intended to run after general binary operator spacing and standard unary fixes.
            pat_numeric_list_spacing_fix = '(\d)\s+([+\-])\s+(\d)';
            rep_numeric_list_spacing_fix = '$1 $2$3'; 
            processedCodePart = regexprep(processedCodePart, pat_numeric_list_spacing_fix, rep_numeric_list_spacing_fix);
            fprintf('DEBUG_SO_NUM_LIST_FIX: line %d, after numeric_list_spacing_fix: "%s"\n', i, processedCodePart);

            % Fix scientific notation (e.g., "1 e - 5" -> "1e-5", or "1e + 5" -> "1e+5").
            % Handles cases where spaces might have been inserted around 'e'/'E' or the exponent's sign.
            processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*([+\-])\s*(\d+)', '$1e$2$3', 'ignorecase'); % e.g. 1e-5 or 1E+10
            processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*(\d+)', '$1e$2', 'ignorecase'); % For exponents without explicit sign, e.g. 1e10
            fprintf('DEBUG_SO_SCIENTIFIC: line %d, after scientific_fix: "%s"\n', i, processedCodePart);
        end

        % Space after comma
        if options.SpaceAfterComma
            processedCodePart = regexprep(processedCodePart, '\s*,\s*', ', '); % Ensures one space after comma, removes spaces before.
            processedCodePart = regexprep(processedCodePart, ', $', ','); % Removes trailing space if comma is at end of code part.
            fprintf('DEBUG_SO_COMMA: line %d, after comma_space: "%s"\n', i, processedCodePart);
        end

        % Ensure space after a semicolon if it's used as a separator within a statement (e.g., in matrix definitions like `[1; 2]`)
        processedCodePart = regexprep(processedCodePart, ';(\S)', '; $1');

        % Explicitly trim processedCodePart after all modifications within this block
        processedCodePart = strtrim(processedCodePart);
    end

    % --- Stage 6: Construct the Beautiful Line ---
    % Refactored logic to explicitly manage trailing whitespace on the content part.
    % processedCodePart at this point is assumed to be trimmed if it passed through Stage 5 modifications.
    
    finalProcessedCodePart = strtrim(processedCodePart); % Ensure it's trimmed.
    contentPart = ''; % This will hold the code + comment part

    % Debug logs for the inputs to contentPart assembly
    fprintf('DEBUG_STAGE6_INPUTS: Line %d, finalProcessedCodePart: "%s"\n', i, finalProcessedCodePart);
    fprintf('DEBUG_STAGE6_INPUTS: Line %d, commentPart: "%s"\n', i, commentPart);

    if isempty(finalProcessedCodePart) && ~isempty(commentPart)
        % Line is effectively comment-only
        contentPart = commentPart;
        % Normalize commentPart: trim trailing spaces, but handle " % " -> "%" or " %"
        if strcmp(strtrim(contentPart), '%') 
            contentPart = '%'; 
        else
            % For comments like " % my comment  " or " %   ", trim to " % my comment" or " %"
            % This also handles cases like "% comment" effectively after strtrim from extractCodeAndCommentInternal
            contentPart = regexprep(contentPart, '\s+$', ''); 
        end
    elseif ~isempty(finalProcessedCodePart) && ~isempty(commentPart)
        % Code and comment
        contentPart = [finalProcessedCodePart, commentPart]; % commentPart should have leading space if needed
        contentPart = regexprep(contentPart, '\s+$', ''); % Trim after combining
    elseif ~isempty(finalProcessedCodePart)
        % Only code; finalProcessedCodePart is already trimmed and commentPart is empty.
        contentPart = finalProcessedCodePart; 
    else
        % Line became effectively empty (was all whitespace or processed to empty)
        contentPart = '';
    end
    
    fprintf('DEBUG_STAGE6_CONTENT: Line %d, contentPart before indent: "%s"\n', i, contentPart);

    % Assign to output lines
    if isempty(contentPart) && isempty(currentIndentStr) % Truly empty line with no indent processing needed
        % This handles lines that were purely whitespace and should remain so if not for indentation.
        % However, if a line is blank, currentIndentStr might be applied by some other logic
        % if it were not for this check. The goal is truly blank lines are '', not '    '.
        % If originalLine was just spaces, and it becomes empty contentPart, it should be empty.
        tempBeautifulLines{i} = ''; 
    else
        % If contentPart is empty but there's an indent (e.g. an empty line in an indented block that shouldn't be there)
        % it will become `currentIndentStr` + `''`. This might be desired for some "empty indented lines".
        % However, the post-processing step for blank lines handles collapsing and preserving,
        % so an empty contentPart here should generally result in just the indent if the line isn't purely blank.
        % The current post-processing handles '' as blank.
        % If contentPart is truly empty (not just whitespace), it means no code or comment.
        if isempty(contentPart)
            tempBeautifulLines{i} = ''; % Make it truly empty to be handled by blank line post-processing
        else
            tempBeautifulLines{i} = [currentIndentStr, contentPart];
        end
    end
    fprintf('DEBUG_STAGE6_FINAL_LINE: Line %d, tempBeautifulLines{i}: "%s"\n', i, tempBeautifulLines{i});

    % --- Stage 7: Update IndentLevel for NEXT line ---
    fprintf('DEBUG_PRE_INDENT_UPDATE: line %d, firstWord="%s", indentLevel before update (for next line)=%d\n', i, firstWord, indentLevel);
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
% At the very start of the loop: for k = 1:length(tempBeautifulLines)
    original_line_from_temp = tempBeautifulLines{k}; 
    currentLineContent = strtrim(original_line_from_temp);
    isCurrentLineBlank = isempty(currentLineContent);

    % === START DEBUG BLOCK 1 ===
    fprintf('DEBUG_POSTPROC_BLANKLINES_ITER_START: Iteration k=%d\n', k);
    fprintf('DEBUG_POSTPROC_BLANKLINES_VARS: original_tempLineUTF8=[%s]\n', native2unicode(uint8(original_line_from_temp), 'UTF-8')); % Log with explicit UTF-8 conversion for clarity
    fprintf('DEBUG_POSTPROC_BLANKLINES_VARS: currentLineContentUTF8=[%s]\n', native2unicode(uint8(currentLineContent), 'UTF-8'));
    fprintf('DEBUG_POSTPROC_BLANKLINES_VARS: isCurrentLineBlank=%d\n', isCurrentLineBlank);
    fprintf('DEBUG_POSTPROC_BLANKLINES_VARS: finalLineCount_before_min_blank_logic=%d\n', finalLineCount);
    fprintf('DEBUG_POSTPROC_BLANKLINES_VARS: options.MinBlankLinesBeforeBlock=%d\n', options.MinBlankLinesBeforeBlock);
    fprintf('DEBUG_POSTPROC_BLANKLINES_VARS: options.PreserveBlankLines=%d\n', options.PreserveBlankLines);
    fprintf('DEBUG_POSTPROC_BLANKLINES_VARS: lastMeaningfulLineWasBlank_before_min_blank_logic=%d\n', lastMeaningfulLineWasBlank);
    % === END DEBUG BLOCK 1 ===

    % MinBlankLinesBeforeBlock logic itself
    if options.MinBlankLinesBeforeBlock > 0 && ~isCurrentLineBlank && finalLineCount > 0
        % === START DEBUG BLOCK 2 ===
        fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_CONDITIONS: Conditions MET for currentLineContentUTF8=[%s]\n', native2unicode(uint8(currentLineContent), 'UTF-8'));
        % === END DEBUG BLOCK 2 ===
        
        firstWordPattern_for_blank_check = ['^\s*(', strjoin(indentKeywords, '|'), ')\b']; % Corrected: use \b
        [codeP_for_blank_check, ~] = extractCodeAndCommentInternal(currentLineContent); 
        firstWordToken_for_blank_check = regexp(codeP_for_blank_check, firstWordPattern_for_blank_check, 'tokens', 'once');

        % === START DEBUG BLOCK 3 ===
        fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_KEYWORD: codeP_for_blank_checkUTF8=[%s]\n', native2unicode(uint8(codeP_for_blank_check), 'UTF-8'));
        if ~isempty(firstWordToken_for_blank_check)
            fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_KEYWORD: firstWordToken={%s}\n', firstWordToken_for_blank_check{1});
        else
            fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_KEYWORD: firstWordToken is EMPTY\n');
        end
        % === END DEBUG BLOCK 3 ===

        if ~isempty(firstWordToken_for_blank_check)
            blanksNeeded = options.MinBlankLinesBeforeBlock;
            % === START DEBUG BLOCK 4 ===
            fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_CALC: Keyword found! blanksNeeded=%d\n', blanksNeeded);
            % === END DEBUG BLOCK 4 ===
            
            numExistingBlanks = 0;
            temp_finalLineCount_for_debug = finalLineCount; 
            if temp_finalLineCount_for_debug > 0 
                % === START DEBUG BLOCK 5 ===
                fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_CALC: Checking existing blanks: current_finalLineCount=%d. Looking from index %d down to 1.\n', temp_finalLineCount_for_debug, temp_finalLineCount_for_debug);
                % === END DEBUG BLOCK 5 ===
                for j = temp_finalLineCount_for_debug:-1:1 
                    line_to_check_for_blank = finalOutputLines{j}; 
                    is_prev_line_blank = isempty(strtrim(line_to_check_for_blank));
                    % === START DEBUG BLOCK 6 (inside inner loop) ===
                    fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_CALC:   Checking finalOutputLines{%d}UTF8=[%s], isBlank=%d\n', j, native2unicode(uint8(line_to_check_for_blank), 'UTF-8'), is_prev_line_blank);
                    % === END DEBUG BLOCK 6 ===
                    if is_prev_line_blank
                        numExistingBlanks = numExistingBlanks + 1;
                    else
                        break; 
                    end
                end
            else
                 % === START DEBUG BLOCK 7 ===
                 fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_CALC: Not checking existing blanks as current_finalLineCount is 0.\n');
                 % === END DEBUG BLOCK 7 ===
            end
            % === START DEBUG BLOCK 8 ===
            fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_CALC: numExistingBlanks found = %d\n', numExistingBlanks);
            % === END DEBUG BLOCK 8 ===

            lines_to_add_count = max(0, blanksNeeded - numExistingBlanks);
            % === START DEBUG BLOCK 9 ===
            fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_ADD: lines_to_add_count = %d\n', lines_to_add_count);
            % === END DEBUG BLOCK 9 ===

            for bl = 1:lines_to_add_count
                finalLineCount = finalLineCount + 1;
                finalOutputLines{finalLineCount} = ''; 
                lastMeaningfulLineWasBlank = true; 
                % === START DEBUG BLOCK 10 (inside inner loop) ===
                fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_ADD: Added a blank line. new_finalLineCount=%d. lastMeaningfulLineWasBlank=true.\n', finalLineCount);
                % === END DEBUG BLOCK 10 ===
            end
        end
    else
        % === START DEBUG BLOCK 11 (else branch) ===
        fprintf('DEBUG_POSTPROC_BLANKLINES_MINBLANK_CONDITIONS: Conditions NOT MET. MinBlankLinesBeforeBlock=%d, isCurrentLineBlank=%d, finalLineCount=%d\n', options.MinBlankLinesBeforeBlock, isCurrentLineBlank, finalLineCount);
        % === END DEBUG BLOCK 11 ===
    end

    % PreserveBlankLines logic
    if isCurrentLineBlank
        if options.PreserveBlankLines % If preserving blank lines
            if ~lastMeaningfulLineWasBlank % And the last meaningful line wasn't also blank (to collapse multiple to one)
                finalLineCount = finalLineCount + 1;
                finalOutputLines{finalLineCount} = ''; % Add the blank line
                lastMeaningfulLineWasBlank = true;
                fprintf('DEBUG_POSTPROC_PRESERVEBLANKS: Adding a preserved blank line. finalLineCount=%d\n', finalLineCount);
            else
                fprintf('DEBUG_POSTPROC_PRESERVEBLANKS: Skipping redundant blank line.\n');
            end
        else
            fprintf('DEBUG_POSTPROC_PRESERVEBLANKS: Not preserving blank line (PreserveBlankLines=false).\n');
        end
    else % Current line is not blank
        finalLineCount = finalLineCount + 1;
        finalOutputLines{finalLineCount} = tempBeautifulLines{k}; % Add the contentful line
        lastMeaningfulLineWasBlank = false;
        fprintf('DEBUG_POSTPROC_ADDCONTENTLINE: Added content line: "%s". finalLineCount=%d\n', native2unicode(uint8(tempBeautifulLines{k}), 'UTF-8'), finalLineCount);
    end
    
    % At the very end of the loop iteration
    % === START DEBUG BLOCK 12 ===
    fprintf('DEBUG_POSTPROC_BLANKLINES_ITER_END: Iteration k=%d, finalLineCount_after_line_process=%d, lastMeaningfulLineWasBlank_after_line_process=%d\n', k, finalLineCount, lastMeaningfulLineWasBlank);
    fprintf('DEBUG_POSTPROC_BLANKLINES_ITER_END: --- End of Iteration k=%d ---\n', k);
    % === END DEBUG BLOCK 12 ===
end
beautifulLines = finalOutputLines(1:finalLineCount)'; % Trim pre-allocated cell array

% --- Optional: Align Assignments ---
% If enabled, this step realigns blocks of consecutive assignment statements.
if options.AlignAssignments && ~isempty(beautifulLines)
    beautifulLines = alignAssignmentBlocksInternal(beautifulLines, options);
end

% --- Format 'arguments' Blocks ---
if options.FormatArgumentsBlock && ~isempty(beautifulLines)
    lineIdx = 1;
    while lineIdx <= length(beautifulLines)
        currentLineOriginal = beautifulLines{lineIdx};
        currentLineTrimmed = strtrim(currentLineOriginal);
        currentLineIndentStr = regexp(currentLineOriginal, '^\s*', 'match', 'once');

        if strcmp(currentLineTrimmed, 'arguments')
            argumentsLineIndex = lineIdx;
            blockContentStartIndex = lineIdx + 1;
            blockContentEndIndex = -1; % Not inclusive of the 'end' line itself
            endLineIndex = -1;

            % Find matching 'end'
            foundEnd = false;
            for j = blockContentStartIndex:length(beautifulLines)
                prospectiveEndLineOriginal = beautifulLines{j};
                prospectiveEndLineTrimmed = strtrim(prospectiveEndLineOriginal);
                prospectiveEndLineIndentStr = regexp(prospectiveEndLineOriginal, '^\s*', 'match', 'once');

                if strcmp(prospectiveEndLineTrimmed, 'end') && strcmp(prospectiveEndLineIndentStr, currentLineIndentStr)
                    blockContentEndIndex = j - 1;
                    endLineIndex = j;
                    foundEnd = true;
                    break;
                end
            end

            if foundEnd && blockContentEndIndex >= blockContentStartIndex
                argumentLinesToFormat = beautifulLines(blockContentStartIndex : blockContentEndIndex);
                
                % Ensure indentChar and indentUnit are available from outer scope
                % (they are defined earlier in code_beautifier.m)
                oneIndentUnit = repmat(indentChar, 1, indentUnit * (options.IndentSize > 0));
                baseContentIndentString = [currentLineIndentStr, oneIndentUnit];
                
                formattedArgumentLines = formatArgumentsBlockInternal(argumentLinesToFormat, options, indentChar, indentUnit, baseContentIndentString);
                
                if length(formattedArgumentLines) == length(argumentLinesToFormat)
                    for k_replace = 1:length(formattedArgumentLines)
                        beautifulLines{blockContentStartIndex + k_replace - 1} = formattedArgumentLines{k_replace};
                    end
                    lineIdx = endLineIndex + 1; % Continue after the 'end' line
                else
                    warning('code_beautifier:ArgBlockLineMismatch', ...
                            'Argument block formatting changed line count. Skipping replacement for block starting at line %d.', argumentsLineIndex);
                    lineIdx = argumentsLineIndex + 1; % Skip 'arguments' line and proceed
                end
            else
                % No matching 'end' found or block empty, just move past 'arguments' line
                lineIdx = argumentsLineIndex + 1;
            end
        else
            lineIdx = lineIdx + 1; % Not an 'arguments' line
        end
    end
end
% --- END: Format 'arguments' Blocks ---

% --- Output Formatting ---
% Convert the cell array of processed lines back to the requested output format ('char' or 'cell').
if strcmpi(options.OutputFormat, 'char')
    beautifulCode = strjoin(beautifulLines, sprintf('\n')); % Join lines into a single char array with '\n' separators
else % 'cell' (default)
    beautifulCode = beautifulLines; % Return as cell array of strings
end
end

% --- Helper function to format 'arguments' blocks ---
function formattedBlockLines = formatArgumentsBlockInternal(blockLines, options, indentChar, indentUnit, baseContentIndentString)
% formatArgumentsBlockInternal Parses and formats lines within an 'arguments' block for alignment.
%
% Syntax:
%   formattedBlockLines = formatArgumentsBlockInternal(blockLines, options, indentChar, indentUnit, baseContentIndentString)
%
% Inputs:
%   blockLines: Cell array of strings. Each cell contains one line from the
%               'arguments' block (excluding the 'arguments' and 'end' lines themselves).
%   options:    Struct containing the beautifier options (e.g., SpaceAroundOperators).
%   indentChar: Character string used for a single indentation unit (e.g., '    ' or '\t').
%   indentUnit: Scalar, number of `indentChar` repetitions for one standard indent level.
%   baseContentIndentString: String, the base indentation to apply to all content lines within the block.
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
    'isCommentOnly', {}, 'isPassThrough', {}); % Removed indentStr, added isPassThrough

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
    parsedArgs(i).isPassThrough = false; % Initialize new field

    trimmedLine = strtrim(line);

    % Preserve empty lines as they are
    if isempty(trimmedLine)
        parsedArgs(i).name = ''; 
        parsedArgs(i).comment = ''; 
        % isCommentOnly and isPassThrough remain false
        continue;
    end

    % Preserve full-line comments
    if startsWith(trimmedLine, '%')
        parsedArgs(i).isCommentOnly = true;
        parsedArgs(i).comment = trimmedLine; % Store the whole trimmed line as comment
        % name remains empty, isPassThrough remains false
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
        originalNameMatch = nameMatch{1}; % Store original match for advancing currentCode
        tempName = strrep(originalNameMatch, char(160), ' '); % Replace non-breaking space
        parsedArgs(i).name = strtrim(tempName); % Final trim
        currentCode = currentCode(length(originalNameMatch)+1:end); % Use length of original match
    else
        % If no name, and not a comment/blank line, it's a pass-through line
        parsedArgs(i).name = '';
        parsedArgs(i).isPassThrough = true; 
        % Store the original trimmed line to preserve it, comment already extracted
        parsedArgs(i).originalLineTrimmed = codePart; % codePart is trimmedLine minus commentPart
        currentCode = ''; % No more to parse for this line if it's pass-through
    end
    currentCode = strtrim(currentCode);

    % 2. Extract Size and Class Specification (e.g., (1,:) char, string) - only if not pass-through
    % This pattern is anchored to the beginning of the remaining currentCode.
    if ~parsedArgs(i).isPassThrough
        sizeClassMatch = regexp(currentCode, ['^', sizeClassPattern], 'tokens', 'once');
        if ~isempty(sizeClassMatch)
            originalSizeClassMatch = sizeClassMatch{1}; % Store original for advancing currentCode
            
            % Step 1: Clean NBSP and trim
            tempSizeClass = strrep(originalSizeClassMatch, char(160), ' '); 
            trimmedSizeClass = strtrim(tempSizeClass);
            
            % Step 2: Standardize comma spacing
            if ~isempty(trimmedSizeClass)
                finalSizeClass = strrep(trimmedSizeClass, ', ', ',');
            else
                finalSizeClass = ''; % Ensure it's empty if trimmedSizeClass is empty
            end
            parsedArgs(i).sizeClass = finalSizeClass; % Store the fully processed string
            
            currentCode = regexprep(currentCode, ['^', regexptranslate('escape', originalSizeClassMatch)], '', 'once'); % Advance using original
        else
            parsedArgs(i).sizeClass = '';
        end
        currentCode = strtrim(currentCode);

        % 3. Extract Validation Functions (e.g., {mustBeNumeric, mustBePositive})
        validatorsMatch = regexp(currentCode, ['^', validatorsPattern], 'tokens', 'once');
        if ~isempty(validatorsMatch)
            originalValidatorsMatch = validatorsMatch{1}; % Store original for advancing
            tempValidators = strrep(originalValidatorsMatch, char(160), ' '); % Clean NBSP
            parsedArgs(i).validators = strtrim(tempValidators); % Trim
            currentCode = regexprep(currentCode, ['^', regexptranslate('escape', originalValidatorsMatch)], '', 'once'); % Advance using original
        else
            parsedArgs(i).validators = '';
        end
        currentCode = strtrim(currentCode);

        % 4. Extract Default Value (e.g., = "default", = 10)
        if startsWith(currentCode, '=') % Check based on potentially spaced currentCode
            defaultMatch = regexp(currentCode, defaultValuePattern, 'tokens', 'once'); % defaultValuePattern is '\s*=\s*(.+)'
            if ~isempty(defaultMatch)
                originalDefaultValueContent = defaultMatch{1}; % This is the content part after " = "
                tempDefault = strrep(originalDefaultValueContent, char(160), ' '); % Clean NBSP
                parsedArgs(i).defaultValue = strtrim(tempDefault); % Trim
            else
                % This case handles "name =" (empty default value)
                parsedArgs(i).defaultValue = ''; 
            end
        else
            parsedArgs(i).defaultValue = ''; 
        end
    end % end if ~parsedArgs(i).isPassThrough
end

% Determine maximum widths for alignment
maxNameLen = 0;
maxSizeClassLen = 0;
maxValidatorsLen = 0;

for i = 1:length(parsedArgs)
    if parsedArgs(i).isCommentOnly || parsedArgs(i).isPassThrough || isempty(parsedArgs(i).name) % Skip special lines
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
        formattedBlockLines{i} = [baseContentIndentString, strtrim(parsedArgs(i).comment)];
        continue;
    end
    
    if isempty(parsedArgs(i).name) && isempty(parsedArgs(i).comment) && ~parsedArgs(i).isPassThrough % Genuine blank line
        formattedBlockLines{i} = ''; % Blank line
        continue;
    end

    if parsedArgs(i).isPassThrough
        % Pass-through lines (like (Repeating)...) get base indent + their original trimmed content
        formattedBlockLines{i} = [baseContentIndentString, strtrim(parsedArgs(i).originalLineTrimmed), parsedArgs(i).comment];
        formattedBlockLines{i} = regexprep(formattedBlockLines{i}, '\s+$', ''); % Trim trailing whitespace
        continue;
    end

    % Argument definition line reconstruction
    lineParts = {};
    lineParts{end+1} = baseContentIndentString; % Use new base indent for content

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
                % lhs extraction with additional cleaning for potentially problematic characters
                tempLhs = codePart(1:equalsIndexInCode-1);
                tempLhs = strrep(tempLhs, char(160), ' '); % Replace non-breaking space (char 160) with regular space
                lhs = strtrim(tempLhs); % Extract Left-Hand Side
                rhs = strtrim(codePart(equalsIndexInCode+1:end)); % Extract Right-Hand Side
                commentPartForAssignment = commentPartExtracted; % Store its trailing comment
            end
        end
    end

    % Decision Logic: Based on whether the line is assignable, a full comment, or other code.
    if isAssignable
        % If it's the first line in a potential block OR its indent matches the last CODE line's indent in the current block:
        if isempty(blockLinesIndices) || (~isempty(blockLinesIndents) && strcmp(currentIndent, blockLinesIndents{end}))
            blockLinesIndices(end+1) = i; % Add line index to block
            blockLinesContent{end+1} = struct('type', 'assignment', ... % Store details
                'lhs', lhs, 'rhs', rhs, ...
                'comment', commentPartForAssignment, ...
                'originalIndex', i, 'indentStr', currentIndent);
            blockLinesIndents{end+1} = currentIndent; % Record indent of this CODE line for the block
            % Update maximum LHS length for alignment calculation
            fprintf('ALIGN_DEBUG_LHS_CALC: Line %d, Original LHS: "%s", Length: %d\n', i, lhs, length(lhs));
            % if options.UseTabs % Tab handling for LHS length is approximate
                maxLhsLen = max(maxLhsLen, length(lhs));
            % else
                % maxLhsLen = max(maxLhsLen, length(lhs));
            % end
            fprintf('ALIGN_DEBUG_LHS_CALC: Line %d, Updated maxLhsLen: %d\n', i, maxLhsLen);
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
            fprintf('ALIGN_DEBUG_LHS_CALC: Line %d (new block), Original LHS: "%s", Length: %d\n', i, lhs, length(lhs));
            % if options.UseTabs
                maxLhsLen = max(maxLhsLen, length(lhs));
            % else
                % maxLhsLen = max(maxLhsLen, length(lhs));
            % end
            fprintf('ALIGN_DEBUG_LHS_CALC: Line %d (new block), Updated maxLhsLen: %d\n', i, maxLhsLen);
        end
    elseif isFullCommentLine
        % If the line is a full comment:
        % And a block is active AND the comment's indent matches the block's code line indent:
        if ~isempty(blockLinesIndices) && ~isempty(blockLinesIndents) && strcmp(currentIndent, blockLinesIndents{end})
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
        fprintf('ALIGN_DEBUG_APPLY: Original Index: %d, LHS: "%s", Length(LHS): %d\n', idx, item.lhs, length(item.lhs));
        fprintf('ALIGN_DEBUG_APPLY: Received maxLhsLen: %d\n', maxLhsLen);

        blockIndent = item.indentStr; % Use the indent stored with this specific assignment line

        % Calculate number of spaces needed to align this line's '=' with `maxLhsLen`.
        numSpacesBeforeEquals = maxLhsLen - length(item.lhs);
        fprintf('ALIGN_DEBUG_APPLY: Calculated numSpacesBeforeEquals: %d\n', numSpacesBeforeEquals);
        
        spacesBeforeEqualsStr = repmat(' ', 1, numSpacesBeforeEquals); % String of spaces
        fprintf('ALIGN_DEBUG_APPLY: Length of spacesBeforeEqualsStr: %d\n', length(spacesBeforeEqualsStr));

        % Construct the new aligned line.
        if options.SpaceAroundOperators % Control spacing around '='
            fprintf('ALIGN_DEBUG_APPLY: Constructing line with " = "\n');
            newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, ' = ', item.rhs, item.comment];
        else
            fprintf('ALIGN_DEBUG_APPLY: Constructing line with "="\n');
            newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, '=', item.rhs, item.comment];
        end
        lines{idx} = regexprep(newLine, '\s+$', ''); % Update the line in the main `lines` cell array, trim trailing whitespace.
        fprintf('ALIGN_DEBUG_APPLY: Final aligned line for index %d: "%s"\n', idx, lines{idx});
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
    if strcmpi(optName, 'StylePreset')
        continue; % Skip StylePreset, it's handled specially in parseConfigFile
    end
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
        % elseif strcmp(optName, 'StylePreset') % StylePreset option itself can be in config - This line is now removed
        %     knownInfo.(optName).validator = @validateStylePresetConfig; % Use a slightly different validator for config context
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

        % Handle StylePreset as a special key
        if strcmpi(key, 'StylePreset')
            if isempty(valueStr)
                warning('code_beautifier:EmptyStylePresetValueInConfigFile', ...
                    'Skipping StylePreset on line %d in .mbeautifyrc because its value is empty.', lineNumber);
                continue;
            end
            % Basic validation for StylePreset value being a string is implicitly handled
            % by its usage. More specific validation (e.g. known preset names) happens later.
            parsedOptions.ConfigFileStylePreset = valueStr; % Store it directly
            continue; % Move to the next line in the config file
        end

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

        if isempty(canonicalKey) % If the key is not a known option (and not StylePreset).
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
                    % NOTE: StylePreset is handled before this block, so no special strcmp check for it here.
                    if isfield(info, 'validator') && ~info.validator(parsedValue)
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
end
