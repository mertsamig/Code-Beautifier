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
rawCode = ''; 
try
    editorDoc = matlab.desktop.editor.getActive();
    if isempty(editorDoc)
        error('code_beautifier:NoActiveEditor', 'No active editor document found. Please open a script or select an editor window.');
    end
    activeCodeText = editorDoc.Text;
    if isempty(strtrim(activeCodeText))
        error('code_beautifier:ActiveEditorEmpty', 'The active editor document is empty or contains only whitespace.');
    end
    rawCode = activeCodeText; 
catch ME
    rethrow(ME);
end

% --- Style Presets Definition ---
stylePresets = struct();
stylePresets.Default = struct(...
    'StylePreset', 'Default', ... 
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
    'OutputFormat', 'char', ...
    'SpaceInsideParentheses', false, ... % New option
    'MaxBlankLinesInCode', 1 ... % New option
    );
stylePresets.MathWorksStyle = struct(...
    'StylePreset', 'MathWorksStyle', ... % FIXED: Added StylePreset field
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
    'FormatArgumentsBlock', false, ...
    'OutputFormat', 'char', ... % Added OutputFormat for consistency, will be overridden by Default if not specified
    'SpaceInsideParentheses', false, ... % New option
    'MaxBlankLinesInCode', 1 ... % New option (MathWorks style might keep 1)
    );
stylePresets.CompactStyle = struct(...
    'StylePreset', 'CompactStyle', ... % FIXED: Added StylePreset field
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
    'FormatArgumentsBlock', false, ...
    'OutputFormat', 'char', ... % Added OutputFormat for consistency
    'SpaceInsideParentheses', false, ... % New option
    'MaxBlankLinesInCode', 0 ... % New option (Compact style might remove internal blank lines)
    );

% --- Determine Effective Defaults ---
effectiveDefaults = stylePresets.Default;

knownOptionsInfo = getKnownOptionsInfo(stylePresets.Default);
configFilePath = fullfile(pwd, '.mbeautifyrc');
configFileOptions = struct();
if exist(configFilePath, 'file')
    configFileOptions = parseConfigFile(configFilePath, knownOptionsInfo);
end

configDefinedPresetName = ''; 
if isfield(configFileOptions, 'ConfigFileStylePreset') && ~isempty(configFileOptions.ConfigFileStylePreset)
    try
        validateStylePreset(configFileOptions.ConfigFileStylePreset); 
        tempConfigPresetName = char(configFileOptions.ConfigFileStylePreset); 

        validPresetNamesForConfig = fieldnames(stylePresets);
        isKnownConfigPreset = false;
        canonicalConfigPresetName = '';
        for vp_idx = 1:length(validPresetNamesForConfig)
            if strcmpi(tempConfigPresetName, validPresetNamesForConfig{vp_idx})
                canonicalConfigPresetName = validPresetNamesForConfig{vp_idx}; 
                isKnownConfigPreset = true;
                break;
            end
        end

        if isKnownConfigPreset 
            configDefinedPresetName = canonicalConfigPresetName; 
            presetSettingsToApply = stylePresets.(configDefinedPresetName);
            fieldsToUpdate = fieldnames(presetSettingsToApply);
            for k_f = 1:length(fieldsToUpdate)
                fieldName = fieldsToUpdate{k_f};
                effectiveDefaults.(fieldName) = presetSettingsToApply.(fieldName);
            end
        else
            warning('code_beautifier:UnknownStylePresetInConfigFile', ...
                'Unknown StylePreset "%s" in .mbeautifyrc. This preset will not be applied. Using defaults from "%s" or prior settings.', ...
                configFileOptions.ConfigFileStylePreset, effectiveDefaults.StylePreset);
        end
    catch ME_config_preset
        warning('code_beautifier:InvalidStylePresetInConfigFile', ...
            'Invalid StylePreset "%s" in .mbeautifyrc: %s. Ignoring this preset. Using defaults from "%s" or prior settings.', ...
            configFileOptions.ConfigFileStylePreset, ME_config_preset.message, effectiveDefaults.StylePreset);
    end
end

fieldsToUpdate = fieldnames(configFileOptions);
for k_f = 1:length(fieldsToUpdate)
    fieldName = fieldsToUpdate{k_f};
    if strcmpi(fieldName, 'ConfigFileStylePreset')
        continue; 
    end
    if isfield(effectiveDefaults, fieldName) 
        effectiveDefaults.(fieldName) = configFileOptions.(fieldName);
    else
        warning('code_beautifier:UnknownOptionInConfigFileProcessing', ...
                'Ignoring unknown option "%s" from config file during effective defaults determination.', fieldName);
    end
end

directArgPresetNameInput = ''; 
for k_v = 1:2:length(varargin)
    if strcmpi(varargin{k_v}, 'StylePreset') && k_v + 1 <= length(varargin)
        directArgPresetNameInput = char(varargin{k_v+1});
        break;
    end
end

if ~isempty(directArgPresetNameInput) 
    canonicalDirectArgPresetName = ''; 
    try
        validateStylePreset(directArgPresetNameInput); 
        
        validPresetNamesForDirect = fieldnames(stylePresets);
        isKnownDirectPreset = false;
        for vp_idx = 1:length(validPresetNamesForDirect)
            if strcmpi(directArgPresetNameInput, validPresetNamesForDirect{vp_idx})
                canonicalDirectArgPresetName = validPresetNamesForDirect{vp_idx}; 
                isKnownDirectPreset = true;
                break;
            end
        end

        if isKnownDirectPreset
            presetSettingsToApply = stylePresets.(canonicalDirectArgPresetName);
            fieldsToUpdate = fieldnames(presetSettingsToApply);
            for k_f = 1:length(fieldsToUpdate)
                fieldName = fieldsToUpdate{k_f};
                effectiveDefaults.(fieldName) = presetSettingsToApply.(fieldName);
            end
        else
            effectiveDefaults.StylePreset = directArgPresetNameInput; 
        end
    catch ME_direct_preset %#ok<NASGU>
        effectiveDefaults.StylePreset = directArgPresetNameInput;
    end
end

% --- Input Parsing ---
options = effectiveDefaults;

if isempty(varargin)
    options.rawCode = rawCode;
else
    p = inputParser;
    addRequired(p, 'rawCode', @(x) (ischar(x) || isstring(x) || iscellstr(x)));

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
    addParameter(p, 'SpaceInsideParentheses', options.SpaceInsideParentheses, @(x) islogical(x) && isscalar(x)); % New option
    addParameter(p, 'MaxBlankLinesInCode', options.MaxBlankLinesInCode, @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x)==x); % New option

    parse(p, rawCode, varargin{:});
    options = p.Results; 
end

if ~isempty(options.StylePreset)
    currentPresetVal = char(options.StylePreset);
    validPresetNames = fieldnames(stylePresets); 
    for vp_idx = 1:length(validPresetNames)
        if strcmpi(currentPresetVal, validPresetNames{vp_idx}) 
            options.StylePreset = validPresetNames{vp_idx}; 
            break;
        end
    end
end

if options.UseTabs
    indentChar = sprintf('\t'); 
    indentUnit = 1;             
else
    indentChar = ' '; 
    indentUnit = options.IndentSize; 
end

if ischar(rawCode) 
    lines = strsplit(rawCode, {'\r\n', '\n', '\r'}, 'CollapseDelimiters', false)'; 
elseif isstring(rawCode) 
    if isscalar(rawCode) 
        lines = strsplit(rawCode, {'\r\n', '\n', '\r'}, 'CollapseDelimiters', false)';
    else 
        lines = cellstr(rawCode); 
    end
else 
    lines = rawCode;
end

% --- Keywords Definitions ---
indentKeywords     = {'if', 'for', 'while', 'switch', 'try', 'parfor', 'function', 'classdef', 'properties', 'methods', 'events', 'arguments'}; 
dedentKeywords     = {'end'}; 
midBlockKeywords   = {'elseif', 'else', 'catch', 'case', 'otherwise'}; 
allBlockCtrlKeywords = [indentKeywords, dedentKeywords, midBlockKeywords]; 
% firstWordPattern   = ['^\s*(', strjoin(allBlockCtrlKeywords, '|'), ')\b']; % Not directly used, firstWord logic is different

% --- DEBUG: Print Final Options ---
% fprintf('DEBUG: options.StylePreset = %s\n', char(options.StylePreset)); 
% fprintf('DEBUG: options.IndentSize = %d\n', options.IndentSize);
% fprintf('DEBUG: options.UseTabs = %d\n', options.UseTabs);
% fprintf('DEBUG: options.SpaceAroundOperators = %d\n', options.SpaceAroundOperators);
% fprintf('DEBUG: options.SpaceAfterComma = %d\n', options.SpaceAfterComma);
% fprintf('DEBUG: options.ContinuationIndentOffset = %d\n', options.ContinuationIndentOffset);
% fprintf('DEBUG: options.PreserveBlankLines = %d\n', options.PreserveBlankLines);
% fprintf('DEBUG: options.MinBlankLinesBeforeBlock = %d\n', options.MinBlankLinesBeforeBlock);
% fprintf('DEBUG: options.RemoveRedundantSemicolons = %d\n', options.RemoveRedundantSemicolons);
% fprintf('DEBUG: options.AddSemicolonsToStatements = %d\n', options.AddSemicolonsToStatements);
% fprintf('DEBUG: options.AlignAssignments = %d\n', options.AlignAssignments);
% fprintf('DEBUG: options.FormatArgumentsBlock = %d\n', options.FormatArgumentsBlock);
% fprintf('DEBUG: options.OutputFormat = %s\n', char(options.OutputFormat)); 
    
% --- Main Processing Loop ---
indentLevel = 0; 
tempBeautifulLines = cell(length(lines), 1); 
inBlockComment = false; 
previousLineEndedWithContinuation = false; 
previousLineActualIndentStr = ''; 

inSwitchBlockDepth = 0; 
inCaseBody = false;     
isFormattingDisabled = false; % State for selective formatting

% Define markers
markerOff = 'beautify_off'; % User writes '% beautify_off'
markerOn = 'beautify_on';   % User writes '% beautify_on'

for i = 1:length(lines) 
    originalLine = lines{i}; 
    trimmedOriginalLine = strtrim(originalLine); 
    
    % The lineData struct holds comprehensive information about each line being processed.
    % It is populated progressively as the line undergoes various formatting stages.
    % Fields include:
    %   originalLine: The raw, unprocessed line string from the input.
    %   trimmedOriginalLine: The originalLine after leading/trailing whitespace is removed.
    %   isBlockCommentBoundaryStart: True if the line starts a block comment (e.g., '%{').
    %   isBlockCommentBoundaryEnd: True if the line ends a block comment (e.g., '%}').
    %   isBlockCommentContent: True if the line is part of a block comment's content, including the boundary lines.
    %   isBlankLine: True if the original trimmed line was empty.
    %   codePart: The extracted code segment from the trimmedOriginalLine, after stripping any trailing comment.
    %   commentPart: The extracted comment segment from the trimmedOriginalLine (e.g., ' % my comment').
    %   firstWord: The first significant keyword (e.g., 'if', 'for', 'end') or token found in the codePart. Used for indent logic.
    %   effectiveIndentLevel: The calculated indentation level (number of indent units) for this line.
    %   effectiveIndentString: The actual indentation string (composed of spaces or tabs) to be prepended to the line.
    %   processedCodePart: The codePart after applying formatting rules like operator spacing, semicolon adjustments, etc.
    %   lineEndsWithContinuation: True if the processedCodePart ends with '...' indicating line continuation.
    %   formattedLine: The constructed line string after applying indentation and processing to code/comment parts. This is the primary output for this line before potential block-level adjustments like assignment alignment.
    %   formattingSkipped: True if this line was skipped due to a '% beautify_off' directive active for this line.
    lineData = struct(...
        'originalLine', originalLine, ...
        'trimmedOriginalLine', trimmedOriginalLine, ...
        'isBlockCommentBoundaryStart', false, ...
        'isBlockCommentBoundaryEnd', false, ...
        'isBlockCommentContent', false, ...
        'isBlankLine', false, ...
        'codePart', '', ...
        'commentPart', '', ...
        'firstWord', '', ...
        'effectiveIndentLevel', 0, ...
        'effectiveIndentString', '', ...
        'processedCodePart', '', ... % Code part after initial processing (semicolons, spacing)
        'lineEndsWithContinuation', false, ...
        'formattedLine', '', ...
        'formattingSkipped', false); % New field

    % Handle selective formatting state for the CURRENT line
    % The decision to skip formatting applies to the current line based on the state *before* processing its comment.
    lineData.formattingSkipped = isFormattingDisabled;

    if startsWith(trimmedOriginalLine, '%{') 
        inBlockComment = true;
        lineData.isBlockCommentBoundaryStart = true;
        lineData.isBlockCommentContent = true; % The '%{' line is part of the content
        baseIndentStr = repmat(indentChar, 1, indentLevel * indentUnit);
        lineData.effectiveIndentString = baseIndentStr;
        lineData.formattedLine = [baseIndentStr, trimmedOriginalLine];
        lineData.codePart = trimmedOriginalLine; % Treat whole line as "code" for this purpose
        previousLineEndedWithContinuation = false; % Block comment start resets continuation
        tempBeautifulLines{i} = lineData;
        continue; 
    elseif inBlockComment 
        lineData.isBlockCommentContent = true;
        % The multiplier `(options.IndentSize > 0)` was removed in a previous commit for `baseIndentStr` here.
        % It should be `indentLevel * indentUnit`.
        baseIndentStr = repmat(indentChar, 1, indentLevel * indentUnit); 
        if endsWith(trimmedOriginalLine, '%}') 
            inBlockComment = false;
            lineData.isBlockCommentBoundaryEnd = true;
        end
        lineData.effectiveIndentString = baseIndentStr;
        lineData.formattedLine = [baseIndentStr, originalLine]; % Use originalLine to preserve internal spacing
        lineData.codePart = originalLine; % Treat whole line as "code"
        previousLineEndedWithContinuation = false; % Block comment content resets continuation
        tempBeautifulLines{i} = lineData;
        continue; 
    end

    % Process current line's comment for beautify_off/on markers
    % This will affect the isFormattingDisabled state for the *next* line.
    [tempCodePartForMarkerCheck, tempCommentPartForMarkerCheck] = extractCodeAndCommentInternal(trimmedOriginalLine);
    
    % Trim comment further to get the actual text: " % my comment " -> "my comment"
    coreCommentText = '';
    if ~isempty(tempCommentPartForMarkerCheck)
        % remove " % " or " %" or "% " or "%" from the beginning
        coreCommentText = strtrim(regexprep(tempCommentPartForMarkerCheck, '^\s*%\s*', ''));
    end

    if strcmp(coreCommentText, markerOff)
        isFormattingDisabled = true;
        % The current line with the marker IS still formatted.
    elseif strcmp(coreCommentText, markerOn)
        isFormattingDisabled = false;
        % The current line with the marker IS still formatted.
    end

    if lineData.formattingSkipped && ~lineData.isBlockCommentBoundaryStart && ~lineData.isBlockCommentContent && ~lineData.isBlockCommentBoundaryEnd
        lineData.formattedLine = originalLine; % Preserve original line completely
        lineData.codePart = trimmedOriginalLine; % For safety, though might not be used if formattedLine is directly used
        lineData.processedCodePart = trimmedOriginalLine;
        lineData.commentPart = ''; % No separate comment handling if skipped
        % Do NOT update previousLineEndedWithContinuation or previousLineActualIndentStr based on this unformatted line
        % Do NOT update indentLevel based on this unformatted line's content
        tempBeautifulLines{i} = lineData;
        continue; % Skip all other formatting for this line
    end
    
    % If formatting is not skipped for this line, proceed as normal
    if isempty(trimmedOriginalLine)
        lineData.isBlankLine = true;
        lineData.formattedLine = '';
        previousLineEndedWithContinuation = false; % Reset for blank lines
        tempBeautifulLines{i} = lineData;
        continue; 
    end

    % Re-extract if not done above or if formatting is active for this line.
    % If formattingSkipped was true, we'd have continued. So this line is being formatted.
    [codeP, commentP] = extractCodeAndCommentInternal(trimmedOriginalLine);
    lineData.codePart = codeP; 
    lineData.commentPart = commentP; 

    tempWords = strsplit(strtrim(lineData.codePart)); 
    if ~isempty(tempWords) && ~isempty(tempWords{1})
        potentialFirstWord = tempWords{1};
        % Strip trailing non-alphanumeric_OR_underscore chars (e.g. from 'function(arg)')
        potentialFirstWord = regexprep(potentialFirstWord, '[^a-zA-Z_0-9].*$', ''); 
        if ismember(potentialFirstWord, allBlockCtrlKeywords)
            lineData.firstWord = potentialFirstWord;
        end
    end
    firstWord = lineData.firstWord; 

    currentLineEffectiveIndentLevel = indentLevel; 
    if isempty(lineData.codePart) && ~isempty(lineData.commentPart) 
        if inCaseBody 
            currentLineEffectiveIndentLevel = currentLineEffectiveIndentLevel + 1;
        end
        % Effective indent level for comment-only lines is current indentLevel
    elseif ismember(firstWord, dedentKeywords) 
        currentLineEffectiveIndentLevel = max(0, indentLevel - 1); 
    elseif ismember(firstWord, midBlockKeywords) 
        if ismember(firstWord, {'case', 'otherwise'})
            % 'case' and 'otherwise' themselves are at the parent switch's indent level.
            % Content *after* them will be indented further.
            inCaseBody = true; % Flag that subsequent lines are in case body
            currentLineEffectiveIndentLevel = max(0, indentLevel - 1); % Align with parent 'switch'
        else % elseif, else, catch
            currentLineEffectiveIndentLevel = max(0, indentLevel - 1);
            inCaseBody = false; % Reset for else/elseif/catch
        end
    end
    
    % For non-keyword lines within a 'case' or 'otherwise' body (but not for 'case'/'otherwise' themselves)
    if inCaseBody && ~ismember(firstWord, {'case', 'otherwise'}) && ~isempty(lineData.codePart)
        currentLineEffectiveIndentLevel = currentLineEffectiveIndentLevel + 1;
    end
    lineData.effectiveIndentLevel = currentLineEffectiveIndentLevel;

    currentIndentStr = repmat(indentChar, 1, currentLineEffectiveIndentLevel * indentUnit);
    if previousLineEndedWithContinuation
        currentIndentStr = [previousLineActualIndentStr, ... 
            repmat(indentChar, 1, options.ContinuationIndentOffset * indentUnit)]; 
    end
    lineData.effectiveIndentString = currentIndentStr;
    previousLineActualIndentStr = currentIndentStr; % Save for potential next continuation

    processedCodePart = lineData.codePart; 
    if ~isempty(processedCodePart)
        % Apply semicolon and operator/comma spacing rules
        if options.RemoveRedundantSemicolons
            processedCodePart = regexprep(processedCodePart, ';(\s*;)+', ';'); 
            if strcmp(firstWord, 'end') && endsWith(strtrim(processedCodePart), ';')
                tempTrimmed = strtrim(processedCodePart);
                if length(tempTrimmed) > 1 && ~ismember(tempTrimmed(end-1), {')', ']', '}'}) 
                    processedCodePart = strtrim(tempTrimmed(1:end-1)); 
                end
            end
        end

        if options.AddSemicolonsToStatements 
            % Regex: '(?<![=<>~.\s])=(?![=])'
            % Purpose: Detects if the line contains an assignment operator '=' that is not part of a comparison operator (e.g. '==', '>=', '<=', '~=').
            % Characteristics: Uses negative lookbehind '(?<!...)' and negative lookahead '(?!...)' to ensure the '=' is standalone.
            %                  It also checks that the character before '=' is not a dot (for struct field access like obj.prop=val) or space (already handled by other rules ideally).
            % Contribution: Helps identify lines that are assignments, which typically do not need an automatically added semicolon if they are intended to suppress output (user adds it).
            isAssignment = ~isempty(regexp(processedCodePart, '(?<![=<>~.\s])=(?![=])', 'once')); 

            isKeywordLine = ~isempty(firstWord); % `firstWord` is already identified (e.g. 'if', 'for', 'function')

            endsWithContOrSemi = endsWith(strtrim(processedCodePart), '...') || endsWith(strtrim(processedCodePart), ';'); 

            % Regex: '\w\s*\(.*\)'
            % Purpose: Identifies expressions that look like function calls, e.g., 'myFunction(arg1, arg2)' or 'disp Hello'.
            % Characteristics: Matches a word character '\w', followed by optional whitespace '\s*', an opening parenthesis '\(', any characters '.*', and a closing parenthesis '\)'.
            %                  This is a broad match for "name(...)" patterns.
            % Contribution: Such function calls, if they are statements themselves (not assignments, not keywords) and don't end with ';' or '...', should get a semicolon to suppress output.
            isFunctionCallLike = ~isempty(regexp(processedCodePart, '\w\s*\(.*\)', 'once')); 

            % Regex 1: '\w' (checks if there's any word character)
            % Regex 2: '^\s*\w+\s*$' (checks if the line is ONLY a single word, possibly with whitespace)
            % Purpose: Identifies if the line is a simple expression (like 'a + b', 'x && y') but not just a single variable name (like 'myVar').
            % Characteristics: `isSimpleExpression` is true if the line contains at least one word character, AND it's not solely a single word.
            %                  A single word on a line (e.g., a variable name to display its value) should not get a semicolon.
            % Contribution: Helps target expressions that produce output and would benefit from a semicolon, while avoiding single variables.
            isSimpleExpression = ~isempty(regexp(processedCodePart, '\w', 'once')) && ... 
                isempty(regexp(processedCodePart,'^\s*\w+\s*$', 'once')); 

            % Condition to add semicolon:
            % - Not an assignment.
            % - Not a line starting with a block control keyword.
            % - Doesn't already end with '...' or ';'.
            % - AND it's either a function-like call or a simple expression (that's not just a single variable).
            if ~isAssignment && ~isKeywordLine && ~endsWithContOrSemi && (isFunctionCallLike || isSimpleExpression)
                processedCodePart = [processedCodePart, ';']; 
            end
        end

        if options.SpaceAroundOperators
            relationalOps = {'==', '~=', '<=', '>=', '<', '>'}; 
            for k_rel = 1:length(relationalOps)
                op_rel = relationalOps{k_rel};
                escaped_op_rel = regexptranslate('escape', op_rel);
                % Pattern: Non-whitespace, optional spaces, relational operator, optional spaces, non-whitespace.
                % Example: A<B, A<=B, A == B
                pat_rel_robust = ['(\S)\s*', escaped_op_rel, '\s*(\S)']; 
                rep_rel = ['$1 ', op_rel, ' $2'];
                % Target: Relational operators (==, ~=, <=, >=, <, >).
                % Purpose: Ensures a single space on each side of relational operators for consistent readability.
                % Approach: Matches the operator and the non-whitespace characters immediately surrounding it,
                % then reinserts them with single spaces around the operator.
                processedCodePart = regexprep(processedCodePart, pat_rel_robust, rep_rel);
            end
            
            % Pattern: Start of line, 'function', potential output args, '=', rest of function definition.
            % Example: function myOutput = myFunction(input)
            func_def_pat_v3 = '^(\s*function\s+[^=]+?)\s*=\s*(.+)$'; 
            func_def_rep_v3 = '$1 = $2'; 
            % Target: Equals sign in function definitions with output arguments.
            % Purpose: Standardizes spacing around the '=' in function signatures like 'function out = func(in)'.
            % Approach: Specifically matches 'function ... = ...' to ensure only the assignment part gets spaced,
            % preserving the 'function' keyword and arguments. Applied 'once' per line.
            processedCodePart = regexprep(processedCodePart, func_def_pat_v3, func_def_rep_v3, 'once');
            
            opListGeneral = { ...
                '&&', '||', ... % Logical
                '.*', './', '.\\', '.^', ... % Element-wise arithmetic
                '*', '/', '\\', '^', ... % Arithmetic
                '=' ... % Assignment (general, after function definition handled)
                };
            for op_idx = 1:length(opListGeneral)
                op = opListGeneral{op_idx};
                escaped_op = regexptranslate('escape', op); 
                % Pattern: Non-whitespace, optional spaces, operator, optional spaces, non-whitespace.
                pat = ['(\S)\s*', escaped_op, '\s*(\S)'];
                rep = ['$1 ', op, ' $2']; 
                % Target: General binary operators (logical && ||, element-wise .*, ./, .\, .^, arithmetic *, /, \, ^, and assignment =).
                % Purpose: Ensures a single space on each side of these common binary operators.
                % Approach: Similar to relational operators, matches operator and surrounding non-whitespace,
                % then reinserts with single spaces. This is applied after the specific function definition '=' case.
                processedCodePart = regexprep(processedCodePart, pat, rep);
            end

            % Pattern: Word char or closing paren/bracket/quote, optional spaces, + or -, optional spaces, word char or opening paren/bracket or dot.
            % Example: a+b, c - d, (e)+f
            s1 = '(\w|\)|\]|\'')'; 
            s2 = '(\w|\(|\[|\.)'; 
            pat_binary_plus_minus = [s1, '\s*([+\-])\s*', s2]; 
            rep_binary_plus_minus = '$1 $2 $3'; 
            % Target: Binary plus (+) and minus (-) operators.
            % Purpose: Adds spaces around binary + and - operators. This is applied before unary operator adjustments.
            % Approach: Identifies + or - when preceded by a character that typically ends an operand (word char, ')]''')
            % and followed by a character that typically starts an operand (word char, '([.' ).
            processedCodePart = regexprep(processedCodePart, pat_binary_plus_minus, rep_binary_plus_minus);

            % Pattern: Preceding char (e.g. '(', '[', '{', '=', ',', space, '&', '|'), one or more spaces, + or -, non-whitespace.
            % Example: (= -val), ([ +val])
            unary_fix_class_1 = '[=\(\[\{,\s&|]'; % Corrected: Added &| as per original
            pat_unary_fix_1 = ['(', unary_fix_class_1, ')\s+([+\-])\s*(\w|[\.\(])'];
            % Target: Unary + or - that might have been incorrectly spaced by previous general rules.
            % Purpose: Removes space between certain preceding characters (like '(', '=', or other operators) and a unary +/-.
            % Approach: Matches a character from `unary_fix_class_1`, followed by spaces, then +/- and an operand.
            % Replaces it by removing the spaces, e.g., changes '(  -val)' to '(-val)'.
            processedCodePart = regexprep(processedCodePart, pat_unary_fix_1, '$1$2$3');

            % Pattern: Start of line, + or -, one or more spaces, non-whitespace.
            % Example: +val (at start of code part)
            pat_unary_fix_2 = ['^([+\-])\s+(\w|[\.\(])'];
            % Target: Unary + or - at the beginning of a code segment.
            % Purpose: Removes leading space if a unary +/- at the start of a line was spaced. E.g. "  +val" -> "+val".
            % Approach: Matches start-of-string, +/-, spaces, and an operand. Replaces by removing spaces.
            processedCodePart = regexprep(processedCodePart, pat_unary_fix_2, '$1$2');

            % Pattern: Digit, optional spaces, 'e', optional spaces, sign (+ or -), optional spaces, digits.
            % Example: 1 e - 5, 2.3E + 10
            % Target: Scientific notation like '1e-5' or '2.3e+10' that might have spaces.
            % Purpose: Ensures no spaces within scientific notation numbers (exponent with sign).
            % Approach: Matches a digit, optional spaces around 'e', optional spaces around sign, then digits.
            % Reconstructs it without spaces. Handles both 'e+num' and 'e-num'. Case-insensitive for 'e'.
            processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*([+\-])\s*(\d+)', '$1e$2$3', 'ignorecase'); 
            % Pattern: Digit, optional spaces, 'e', optional spaces, digits.
            % Example: 1 e 5, 2.3E 10
            % Target: Scientific notation like '1e5' (no explicit sign) that might have spaces.
            % Purpose: Ensures no spaces within scientific notation numbers (exponent without sign).
            % Approach: Matches a digit, optional spaces around 'e', then digits. Reconstructs without spaces. Case-insensitive for 'e'.
            processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*(\d+)', '$1e$2', 'ignorecase'); 
        end

        if options.SpaceAfterComma
            processedCodePart = regexprep(processedCodePart, '\s*,\s*', ', '); 
            processedCodePart = regexprep(processedCodePart, ', $', ','); 
        end
        
        if options.SpaceInsideParentheses
            % Add space after ( if not followed by space, another (, or )
            processedCodePart = regexprep(processedCodePart, '\((?=[^\s\)])', '( ');
            % Add space before ) if not preceded by space, another ), or (
            processedCodePart = regexprep(processedCodePart, '(?<=[^\s\(\(])\)', ' )');
            % Handle empty parentheses `()` or `( )` - ensure they become `()` without inner space
            processedCodePart = regexprep(processedCodePart, '\(\s+\)', '()');
        end

        processedCodePart = regexprep(processedCodePart, ';(\S)', '; $1');
    end
    lineData.processedCodePart = strtrim(processedCodePart); % Store the processed code part
    
    % Reconstruct the formatted line string for current stage
    if isempty(lineData.processedCodePart) && ~isempty(lineData.commentPart) 
        lineData.formattedLine = regexprep([currentIndentStr, lineData.commentPart], '\s+$', '');
    elseif ~isempty(lineData.processedCodePart) && ~isempty(lineData.commentPart) 
        lineData.formattedLine = regexprep([currentIndentStr, lineData.processedCodePart, lineData.commentPart], '\s+$', '');
    elseif ~isempty(lineData.processedCodePart) 
        lineData.formattedLine = regexprep([currentIndentStr, lineData.processedCodePart], '\s+$', '');
    else % Both processedCodePart and commentPart are empty (already handled by isBlankLine or block comments)
        lineData.formattedLine = ''; % Should ideally be just currentIndentStr if it was a blank line with indent. But handled by isBlankLine.
    end
    
    lineData.lineEndsWithContinuation = endsWith(strtrim(lineData.processedCodePart), '...');
    previousLineEndedWithContinuation = lineData.lineEndsWithContinuation;
    
    tempBeautifulLines{i} = lineData;

    % Update indentLevel for the next line based on the firstWord of the current line's code part
    % This should only happen if formatting was NOT skipped for this line.
    % If formatting was skipped, indentLevel remains unchanged from the previous formatted line.
    if ~lineData.formattingSkipped && options.IndentSize > 0 % Only change indentLevel if indentation is active & line was formatted
        if ismember(lineData.firstWord, dedentKeywords) 
            current_indentLevel_before_dedent = indentLevel; 
            indentLevel = max(0, indentLevel - 1); 
            if inSwitchBlockDepth > 0 && indentLevel < current_indentLevel_before_dedent
                inSwitchBlockDepth = max(0, inSwitchBlockDepth - 1);
                if inSwitchBlockDepth == 0 
                    inCaseBody = false; 
                end
            end
        elseif ismember(lineData.firstWord, midBlockKeywords) 
            if ismember(lineData.firstWord, {'case', 'otherwise'})
                % indentLevel for 'case'/'otherwise' line itself is parent's level (already set).
                % inCaseBody flag is set prior to this block.
            else % elseif, else, catch
                % These keywords are dedented, then effectively re-indented with the block.
                % The currentLineEffectiveIndentLevel is already set to (indentLevel - 1).
                % The indentLevel for the *next* line will be (indentLevel - 1) + 1 = indentLevel.
                % No change to indentLevel for next line based on these keywords alone,
                % but they do reset inCaseBody.
                inCaseBody = false; % Not strictly necessary here as it's reset by 'if' etc.
            end
        elseif ismember(lineData.firstWord, indentKeywords) 
            if strcmp(lineData.firstWord, 'switch')
                inSwitchBlockDepth = inSwitchBlockDepth + 1; 
            end
            indentLevel = indentLevel + 1; 
        end
    end
end

% Convert cell array of structs to cell array of formatted line strings for further processing
formattedLineStrings = cell(length(tempBeautifulLines), 1);
for k_line = 1:length(tempBeautifulLines)
    if isstruct(tempBeautifulLines{k_line}) % Ensure it's a struct (might be empty if original line was empty and not processed)
        formattedLineStrings{k_line} = tempBeautifulLines{k_line}.formattedLine;
    else
        formattedLineStrings{k_line} = ''; % Should have been populated as a struct
    end
end


% --- Post Processing: Blank Lines and MinBlankLinesBeforeBlock ---
% This section now operates on formattedLineStrings derived from lineData.formattedLine
finalOutputLines = cell(1, length(formattedLineStrings) + options.MinBlankLinesBeforeBlock * length(formattedLineStrings) + length(formattedLineStrings)); % Max possible size
finalLineCount = 0;
consecutiveBlankLineCount = 0; % For MaxBlankLinesInCode

for k = 1:length(formattedLineStrings) 
    currentLineStruct = tempBeautifulLines{k}; % Get the struct for more info
    currentLineContent = formattedLineStrings{k}; % Use the already formatted string
    isCurrentLineActuallyBlank = currentLineStruct.isBlankLine || isempty(strtrim(currentLineContent)); % Consider original blank or if formatting made it effectively blank (e.g. only indent)

    % 1. Handle MinBlankLinesBeforeBlock (only if current line is not blank itself)
    if options.MinBlankLinesBeforeBlock > 0 && ~isCurrentLineActuallyBlank && finalLineCount > 0
        firstWordToken_for_blank_check_str = '';
        if ~currentLineStruct.isBlockCommentBoundaryStart && ~currentLineStruct.isBlockCommentContent
            firstWordToken_for_blank_check_str = currentLineStruct.firstWord;
        end
        
        if ismember(firstWordToken_for_blank_check_str, indentKeywords)
            % Count existing blank lines immediately preceding this non-blank line
            numExistingBlanksImmediatelyBefore = 0;
            for j = finalLineCount:-1:max(1, finalLineCount - options.MinBlankLinesBeforeBlock -1) % Look back a bit
                if isempty(strtrim(finalOutputLines{j}))
                    numExistingBlanksImmediatelyBefore = numExistingBlanksImmediatelyBefore + 1;
                else
                    break; % Hit a non-blank line
                end
            end
            
            blanksToAdd = options.MinBlankLinesBeforeBlock - numExistingBlanksImmediatelyBefore;
            for bl = 1:blanksToAdd
                if finalLineCount > 0 && ~isempty(strtrim(finalOutputLines{finalLineCount})) % Ensure previous wasn't already blank from other logic
                     finalLineCount = finalLineCount + 1;
                     finalOutputLines{finalLineCount} = '';
                elseif finalLineCount == 0 % If it's the first line, add blank line
                     finalLineCount = finalLineCount + 1;
                     finalOutputLines{finalLineCount} = '';
                end
            end
        end
    end

    % 2. Handle current line (blank or content) based on PreserveBlankLines and MaxBlankLinesInCode
    if isCurrentLineActuallyBlank
        if options.PreserveBlankLines
            consecutiveBlankLineCount = consecutiveBlankLineCount + 1;
            if consecutiveBlankLineCount <= options.MaxBlankLinesInCode
                finalLineCount = finalLineCount + 1;
                finalOutputLines{finalLineCount} = ''; % Add the blank line (it's empty)
            end
        else
            % Do not add blank line if PreserveBlankLines is false (equivalent to MaxBlankLinesInCode = 0)
            consecutiveBlankLineCount = 0; % Reset counter
        end
    else % Current line has content
        consecutiveBlankLineCount = 0; % Reset counter
        finalLineCount = finalLineCount + 1;
        finalOutputLines{finalLineCount} = currentLineContent; 
    end
end
beautifulLines = finalOutputLines(1:finalLineCount)'; 


% --- Optional: Align Assignments ---
if options.AlignAssignments && ~isempty(beautifulLines) % beautifulLines is cellstr here
    % alignAssignmentBlocksInternal needs to be adapted to use tempBeautifulLines (structs)
    % For now, it will operate on 'beautifulLines' (cellstr) and thus re-parse.
    % TODO: Refactor alignAssignmentBlocksInternal to accept tempBeautifulLines (struct array)
    beautifulLines = alignAssignmentBlocksInternal(beautifulLines, options, tempBeautifulLines); % Pass structs
end

% --- Optional: Format Arguments Blocks ---
if options.FormatArgumentsBlock && ~isempty(beautifulLines) % beautifulLines is cellstr here
    % applyArgumentsBlockFormatting needs to be adapted
    % TODO: Refactor applyArgumentsBlockFormatting to accept tempBeautifulLines (struct array)
    beautifulLines = applyArgumentsBlockFormatting(beautifulLines, options, indentChar, indentUnit, tempBeautifulLines); % Pass structs
end


% --- Output Formatting ---
if strcmpi(options.OutputFormat, 'char')
    beautifulCode = strjoin(beautifulLines, sprintf('\n')); 
else 
    beautifulCode = beautifulLines; % This is now a cellstr of formatted lines
end
end

% --- Helper function to apply 'arguments' block formatting (NEW) ---
% TODO: Modify to accept tempBeautifulLines structs for lines within the block
function lines = applyArgumentsBlockFormatting(lines, options, indentChar, indentUnit, lineStructs)
    if isempty(lines) || ~options.FormatArgumentsBlock 
        return; 
    end
    
    outputLines = lines; % Work on a copy (cellstr)
    
    idx = 1;
    while idx <= length(outputLines)
        % currentLine = outputLines{idx}; % This is a string
        % currentLineStruct = lineStructs{idx}; % This would be the corresponding struct

        % Use firstWord from lineStructs if available, otherwise parse
        firstWordCurrent = '';
        if idx <= length(lineStructs) && isfield(lineStructs{idx}, 'firstWord') && ...
           ~lineStructs{idx}.isBlockCommentBoundaryStart && ~lineStructs{idx}.isBlockCommentContent
            firstWordCurrent = lineStructs{idx}.firstWord;
        else % Fallback for lines not in lineStructs (e.g. if called independently) or block comments
            [codePartCurrentFallback, ~] = extractCodeAndCommentInternal(strtrim(outputLines{idx}));
            tokensCurrentFallback = regexp(codePartCurrentFallback, '^\s*(\w+)', 'tokens', 'once');
            if ~isempty(tokensCurrentFallback)
                firstWordCurrent = tokensCurrentFallback{1};
            end
        end
        
        % [codePartCurrent, ~] = extractCodeAndCommentInternal(strtrim(currentLine));
        % tokensCurrent = regexp(codePartCurrent, '^\s*(\w+)', 'tokens', 'once');
        if strcmp(firstWordCurrent, 'arguments')
            argBlockStartLineIndex = idx;
            argBlockContentLineStructs = {}; % Cell array for structs
            contentLineIndicesInOutputLines = [];
            
            nestingLevel = 1; 
            foundEnd = false;
            
            for j = (idx + 1) : length(outputLines)
                % Determine first word of line j, preferably from lineStructs
                firstWordJ = '';
                isJBlockComment = false;
                if j <= length(lineStructs) && isfield(lineStructs{j}, 'firstWord')
                     isJBlockComment = lineStructs{j}.isBlockCommentBoundaryStart || lineStructs{j}.isBlockCommentContent;
                     if ~isJBlockComment
                        firstWordJ = lineStructs{j}.firstWord;
                     end
                end
                if isempty(firstWordJ) && ~isJBlockComment % Fallback if not in lineStructs or it was empty
                    [codePartJ_fallback, ~] = extractCodeAndCommentInternal(strtrim(outputLines{j}));
                    tokensJ_fallback = regexp(codePartJ_fallback, '^\s*(\w+)', 'tokens', 'once');
                    if ~isempty(tokensJ_fallback)
                        firstWordJ = tokensJ_fallback{1};
                    end
                end

                if strcmp(firstWordJ, 'arguments')
                    nestingLevel = nestingLevel + 1; 
                elseif strcmp(firstWordJ, 'end')
                    nestingLevel = nestingLevel - 1;
                    if nestingLevel == 0
                        if (j-1) >= (idx+1) 
                            contentLineIndicesInOutputLines = (idx+1):(j-1);
                            % Collect corresponding structs for formatArgumentsBlockInternal
                            for k_struct = 1:length(contentLineIndicesInOutputLines)
                                line_idx_in_structs = contentLineIndicesInOutputLines(k_struct);
                                if line_idx_in_structs <= length(lineStructs)
                                    argBlockContentLineStructs{end+1} = lineStructs{line_idx_in_structs};
                                else
                                    % Fallback: create a minimal struct if not found (should not happen in normal flow)
                                    dummyStruct = struct('codePart', strtrim(outputLines{line_idx_in_structs}), 'commentPart', '', 'effectiveIndentString', '');
                                    [dummyStruct.codePart, dummyStruct.commentPart] = extractCodeAndCommentInternal(dummyStruct.codePart);
                                    dummyStruct.effectiveIndentString = regexp(outputLines{line_idx_in_structs}, '^\s*', 'match', 'once');
                                    argBlockContentLineStructs{end+1} = dummyStruct;
                                end
                            end
                        end
                        
                        % Pass cell array of structs to formatArgumentsBlockInternal
                        formattedContentCellStr = formatArgumentsBlockInternal(argBlockContentLineStructs, options, indentChar, indentUnit);
                        
                        for k_block = 1:length(formattedContentCellStr)
                            outputLines{contentLineIndicesInOutputLines(k_block)} = formattedContentCellStr{k_block};
                            % Also update lineStructs if it's being used as the primary source later
                            % lineStructs{contentLineIndicesInOutputLines(k_block)}.formattedLine = formattedContentCellStr{k_block};
                        end
                        
                        idx = j; 
                        foundEnd = true;
                        break; 
                    end
                end
            end 
            if ~foundEnd
                % warning(...);
            end
        end
        idx = idx + 1; 
    end
    lines = outputLines; 
end


% --- Helper function to format 'arguments' blocks ---
% Now accepts a cell array of lineData structs
function formattedBlockLinesCellStr = formatArgumentsBlockInternal(blockLineStructs, options, indentChar, indentUnit) %#ok<INUSD>
if isempty(blockLineStructs)
    formattedBlockLinesCellStr = {};
    return;
end

parsedArgs = repmat(struct(...
    'name', '', 'sizeClass', '', 'validators', '', ...
    'defaultValue', '', 'hasDefaultValue', false, 'comment', '', ...
    'originalLineContent', '', 'isCommentOnly', false, 'baseIndentStr', ''), ...
    length(blockLineStructs), 1);

for i = 1:length(blockLineStructs)
    currentLineStruct = blockLineStructs{i};
    parsedArgs(i).originalLineContent = currentLineStruct.trimmedOriginalLine; % Or .formattedLine if it's post-main loop
    parsedArgs(i).baseIndentStr = currentLineStruct.effectiveIndentString; % This is the indent of the block itself
    
    % Use pre-parsed codePart and commentPart from the struct
    currentCode = currentLineStruct.codePart; 
    parsedArgs(i).comment = currentLineStruct.commentPart;

    if isempty(currentCode) && ~isempty(parsedArgs(i).comment) && startsWith(strtrim(parsedArgs(i).comment),'%')
         % Check if the original trimmed line was purely a comment (e.g. '% just a comment')
         % or if codePart became empty due to some processing error before.
         % If currentLineStruct.codePart was truly empty from extractCodeAndCommentInternal, this is a comment line.
        if isempty(currentLineStruct.codePart) && startsWith(currentLineStruct.trimmedOriginalLine,'%')
            parsedArgs(i).isCommentOnly = true;
        end
    end
    if parsedArgs(i).isCommentOnly
        continue; % Skip further parsing for comment-only lines
    end
    
    % If the line was not a comment, but codePart is empty (e.g. blank line struct)
    if isempty(currentCode)
        continue;
    end

    namePattern = '^\s*([a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*)';
    sizeClassPattern = '\s*((?:\([^\)]*?\))?\s*[a-zA-Z_]\w*)';
    validatorsPattern = '\s*(\{[^\}]*\})';
    defaultValuePattern = '\s*=\s*(.*)';

    nameMatch = regexp(currentCode, namePattern, 'tokens', 'once');
    if ~isempty(nameMatch)
        parsedArgs(i).name = strtrim(nameMatch{1});
        currentCode = currentCode(length(nameMatch{1})+1:end);
    end
    currentCode = strtrim(currentCode); 

    sizeClassMatch = regexp(currentCode, ['^', sizeClassPattern], 'tokens', 'once');
    if ~isempty(sizeClassMatch)
        parsedArgs(i).sizeClass = strtrim(sizeClassMatch{1});
        currentCode = regexprep(currentCode, ['^', regexptranslate('escape', parsedArgs(i).sizeClass)], '', 'once');
    end
    currentCode = strtrim(currentCode);

    validatorsMatch = regexp(currentCode, ['^', validatorsPattern], 'tokens', 'once');
    if ~isempty(validatorsMatch)
        parsedArgs(i).validators = strtrim(validatorsMatch{1});
        currentCode = regexprep(currentCode, ['^', regexptranslate('escape', parsedArgs(i).validators)], '', 'once');
    end
    currentCode = strtrim(currentCode);

    if startsWith(currentCode, '=')
        parsedArgs(i).hasDefaultValue = true; 
        defaultMatch = regexp(currentCode, defaultValuePattern, 'tokens', 'once');
        if ~isempty(defaultMatch) 
            parsedArgs(i).defaultValue = strtrim(defaultMatch{1});
        else
            parsedArgs(i).defaultValue = ''; 
        end
    end
end

maxNameLen = 0;
maxSizeClassLen = 0;
maxValidatorsLen = 0;

for i = 1:length(parsedArgs)
    if parsedArgs(i).isCommentOnly || isempty(parsedArgs(i).name)
        continue;
    end
    maxNameLen = max(maxNameLen, length(parsedArgs(i).name));
    maxSizeClassLen = max(maxSizeClassLen, length(parsedArgs(i).sizeClass));
    maxValidatorsLen = max(maxValidatorsLen, length(parsedArgs(i).validators));
end

formattedBlockLinesCellStr = cell(length(blockLineStructs), 1);
for i = 1:length(parsedArgs)
    if parsedArgs(i).isCommentOnly
        % Use baseIndentStr (indent of the arguments block itself) + the comment content
        % The comment content from struct already includes its original relative spacing/formatting.
        formattedBlockLinesCellStr{i} = [parsedArgs(i).baseIndentStr, parsedArgs(i).comment];
        continue;
    end
    
    % Handle lines that were not comments but did not parse a name (e.g., '(Repeating)...' or malformed)
    % Use their original content with the block's base indent.
    if isempty(parsedArgs(i).name)
        formattedBlockLinesCellStr{i} = [parsedArgs(i).baseIndentStr, parsedArgs(i).originalLineContent];
        continue;
    end

    lineParts = {};
    lineParts{end+1} = parsedArgs(i).baseIndentStr; % Start with the block's base indent

    nameStr = parsedArgs(i).name; % Already trimmed
    namePadding = maxNameLen - length(nameStr);
    lineParts{end+1} = [nameStr, repmat(' ', 1, namePadding)];

    sizeClassStr = parsedArgs(i).sizeClass; % Already trimmed or empty
    if ~isempty(sizeClassStr)
        sizeClassPadding = maxSizeClassLen - length(sizeClassStr);
        lineParts{end+1} = [' ', sizeClassStr, repmat(' ', 1, sizeClassPadding)];
    elseif maxSizeClassLen > 0 
        lineParts{end+1} = repmat(' ', 1, maxSizeClassLen + 1); 
    end

    validatorsStr = parsedArgs(i).validators; % Already trimmed or empty
    if ~isempty(validatorsStr)
        validatorsPadding = maxValidatorsLen - length(validatorsStr);
        lineParts{end+1} = [' ', validatorsStr, repmat(' ', 1, validatorsPadding)];
    elseif maxValidatorsLen > 0 
        lineParts{end+1} = repmat(' ', 1, maxValidatorsLen + 1); 
    end

    if parsedArgs(i).hasDefaultValue
        defaultStr = parsedArgs(i).defaultValue; % Already trimmed
        if options.SpaceAroundOperators
            lineParts{end+1} = [' = ', defaultStr];
        else
            lineParts{end+1} = ['=', defaultStr];
        end
    end

    fullLine = strjoin(lineParts, '');
    fullLine = regexprep(fullLine, '\s+$', ''); % Trim trailing space from assembled parts

    commentStr = parsedArgs(i).comment; % Already processed
    if ~isempty(commentStr)
        if ~isempty(strtrim(fullLine)) % Check if fullLine is not just whitespace
             % Ensure a space before comment if line has content.
             % commentStr already has leading ' % ' or similar if it's not empty.
            fullLine = [fullLine, commentStr]; 
        else 
            % If fullLine is empty/whitespace, use baseIndent + trimmed comment
            fullLine = [parsedArgs(i).baseIndentStr, strtrim(commentStr)];
        end
    end
    formattedBlockLinesCellStr{i} = fullLine; 
end
end

% --- Helper function to align assignment blocks ---
% TODO: Modify to accept tempBeautifulLines (structs) and update structs' formattedLine
function lines = alignAssignmentBlocksInternal(lines, options, lineStructs)
    if isempty(lines), return; end 

blockLinesIndices = [];      
blockItemsToFormat = {}; 
maxLhsLen = 0;               

% indentKeywords is accessible here due to nested function scope.

resetBlockState = @() deal([], {}, 0); 

    % Determine safe iteration range to prevent indexing errors if lines and lineStructs have different lengths
    safeLength = min(length(lines), length(lineStructs));
    if length(lines) ~= length(lineStructs)
        warning('code_beautifier:align_length_mismatch', ...
                'Mismatch between processed lines array length (%d) and line structures array length (%d) in alignment block processing. Will process common range of %d lines. This might indicate an issue in preceding blank line handling or struct generation.', ...
                length(lines), length(lineStructs), safeLength);
    end

    for i = 1:safeLength % Iterate only over the common, safe range
        currentLineStruct = lineStructs{i};
    
    if currentLineStruct.isBlockCommentBoundaryStart || currentLineStruct.isBlockCommentContent || currentLineStruct.isBlankLine || currentLineStruct.formattingSkipped
        if ~isempty(blockItemsToFormat)
            lines = applyAlignmentToBlockInternal(lines, blockItemsToFormat, maxLhsLen, options);
        end
        [blockLinesIndices, blockItemsToFormat, maxLhsLen] = resetBlockState();
        continue;
    end

    currentIndent = currentLineStruct.effectiveIndentString;
    codePart = currentLineStruct.processedCodePart; 
    commentPart = currentLineStruct.commentPart;   
    
    isFullCommentLine = isempty(codePart) && ~isempty(commentPart) && startsWith(strtrim(commentPart),'%');
    isAssignable = false; 
    lhs = ''; rhs = ''; 
    
    if ~isFullCommentLine && ~isempty(codePart)
        % Check if the firstWord of the struct indicates it's a block control keyword line.
        % currentLineStruct.firstWord is non-empty only if it's one of allBlockCtrlKeywords.
        isKeywordLine = ~isempty(currentLineStruct.firstWord); 

        if ~isKeywordLine && ~endsWith(codePart, '...')
            equalsIndices = strfind(codePart, '=');
            finalEqualsIndex = -1;
            if ~isempty(equalsIndices)
                % Filter out ==, >=, <=, ~=
                tempCode = codePart;
                for eqIdx = equalsIndices
                    isComparison = false;
                    if eqIdx > 1 && ismember(tempCode(eqIdx-1), {'=', '~', '<', '>'})
                        isComparison = true;
                    end
                    if eqIdx < length(tempCode) && tempCode(eqIdx+1) == '='
                        isComparison = true;
                    end
                    if ~isComparison
                        finalEqualsIndex = eqIdx;
                        break; % Take the first valid one
                    end
                end
            end

            if finalEqualsIndex > 0
                isAssignable = true;
                lhs = strtrim(codePart(1:finalEqualsIndex-1));
                rhs = strtrim(codePart(finalEqualsIndex+1:end));
            end
        end
    end

    if isAssignable
        if isempty(blockLinesIndices) || strcmp(currentIndent, blockItemsToFormat{end}.indentStr)
            blockLinesIndices(end+1) = i; 
            blockItemsToFormat{end+1} = struct('lhs', lhs, 'rhs', rhs, ...
                'comment', commentPart, 'originalIndex', i, 'indentStr', currentIndent);
            maxLhsLen = max(maxLhsLen, length(lhs));
        else 
            if ~isempty(blockItemsToFormat)
                lines = applyAlignmentToBlockInternal(lines, blockItemsToFormat, maxLhsLen, options);
            end
            [blockLinesIndices, blockItemsToFormat, maxLhsLen] = resetBlockState(); 
            blockLinesIndices(end+1) = i;
            blockItemsToFormat{end+1} = struct('lhs', lhs, 'rhs', rhs, ...
                'comment', commentPart, 'originalIndex', i, 'indentStr', currentIndent);
            maxLhsLen = max(maxLhsLen, length(lhs));
        end
    elseif isFullCommentLine
        if ~isempty(blockItemsToFormat) && strcmp(currentIndent, blockItemsToFormat{end}.indentStr)
            comment_text_for_block = ''; % Default to empty if access is unsafe
            if i <= length(lines)
                comment_text_for_block = lines{i};
            else
                warning('code_beautifier:lines_shortened_for_comment', ...
                        'Attempted to access lines{%d} for comment alignment when lines array was shorter. Original line index was %d. Using original line content if available, or empty.', i, i);
                if i <= length(lineStructs) && isfield(lineStructs{i}, 'originalLine')
                     comment_text_for_block = lineStructs{i}.originalLine; % Get original comment if possible
                end
            end
            blockLinesIndices(end+1) = i; % Add its index
            blockItemsToFormat{end+1} = struct('lhs', '', 'rhs', '', ...
                'comment', comment_text_for_block, 'originalIndex', i, 'indentStr', currentIndent, 'isCommentOnly', true);
        else % This comment line does not continue the current alignment block (different indent or no prior block)
            if ~isempty(blockItemsToFormat) % Finalize the previous block
                lines = applyAlignmentToBlockInternal(lines, blockItemsToFormat, maxLhsLen, options);
            end
            % Reset state for the next block; the current comment line itself won't start a new alignment block.
            [blockLinesIndices, blockItemsToFormat, maxLhsLen] = resetBlockState();
        end
    else % Not assignable, not a full comment line continuing block: breaks the block
        if ~isempty(blockItemsToFormat)
            lines = applyAlignmentToBlockInternal(lines, blockItemsToFormat, maxLhsLen, options);
        end
        [blockLinesIndices, blockItemsToFormat, maxLhsLen] = resetBlockState();
    end
end

if ~isempty(blockItemsToFormat)
    lines = applyAlignmentToBlockInternal(lines, blockItemsToFormat, maxLhsLen, options);
end
end

% Renamed from applyAlignmentToBlock to avoid conflict, and to signify internal use
function lines = applyAlignmentToBlockInternal(lines, blockItems, maxLhsLen, options)
if isempty(blockItems), return; end

for k = 1:length(blockItems) 
    item = blockItems{k};
    idx = item.originalIndex; 

    if isfield(item, 'isCommentOnly') && item.isCommentOnly
        % This line is purely a comment within the alignment block, do not change it.
        % lines{idx} = item.comment; % item.comment here would be the full original comment line
        continue; 
    end
    
    % Only process if it's an actual assignment (i.e., has an LHS)
    if isempty(item.lhs)
        continue;
    end

    blockIndent = item.indentStr; 
    numSpacesBeforeEquals = maxLhsLen - length(item.lhs);
    spacesBeforeEqualsStr = repmat(' ', 1, numSpacesBeforeEquals); 

    if options.SpaceAroundOperators 
        newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, ' = ', item.rhs, item.comment];
    else
        newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, '=', item.rhs, item.comment];
    end
    lines{idx} = regexprep(newLine, '\s+$', ''); 
end
end



function [codeP, commentP] = extractCodeAndCommentInternal(lineStr)
    trimmedLine = strtrim(lineStr);
    codeP = trimmedLine;
    commentP = '';

    len = length(trimmedLine);
    actualCommentStartIdx = -1;

    inSingleQuoteString = false;
    inDoubleQuoteString = false;

    i = 1;
    while i <= len
        char = trimmedLine(i);

        if inSingleQuoteString
            % We are inside a single-quoted string
            if char == ''''
                if i + 1 <= len && trimmedLine(i+1) == '''' % Escaped single quote: ''
                    i = i + 1; % Consume the second quote, remain inSingleQuoteString
                else
                    inSingleQuoteString = false; % End of single-quoted string
                end
            end
            % Any other char (including '%' or '"') is part of the single-quoted string
        elseif inDoubleQuoteString
            % We are inside a double-quoted string
            if char == '"' % Check for double quote character
                if i + 1 <= len && trimmedLine(i+1) == '"' % Escaped double quote: ""
                    i = i + 1; % Consume the second quote, remain inDoubleQuoteString
                else
                    inDoubleQuoteString = false; % End of double-quoted string
                end
            end
            % Any other char (including '%' or '''') is part of the double-quoted string
        else % Not currently in any string literal
            if char == ''''
                % Could be start of a new single-quoted string or a transpose operator
                isLikelyTranspose = false;
                if i > 1 % Transpose cannot be the first character of a trimmed line.
                    prevChar = trimmedLine(i-1);
                    if isstrprop(prevChar, 'alphanum') || prevChar == '_' || prevChar == '.' || ...
                       ismember(prevChar, [')', ']', '}'])
                        if i == len || ismember(trimmedLine(i+1), [' ', '%', ';', ',']) % Check char after
                            isLikelyTranspose = true;
                        end
                    end
                end

                if isLikelyTranspose
                    % It's a transpose operator. It does not change string state.
                else
                    % Not a transpose, so it's a string delimiter.
                    inSingleQuoteString = true;
                    if i + 1 <= len && trimmedLine(i+1) == '''' % Handles s = '''...'; (escaped quote at start of string)
                        i = i + 1; % Consume the second ' of an opening ''
                    end
                end
            elseif char == '"' % Check for double quote character
                % Start of a double-quoted string
                inDoubleQuoteString = true;
                if i + 1 <= len && trimmedLine(i+1) == '"' % Handles s = """..."""; (escaped quote at start of string)
                    i = i + 1; % Consume the second " of an opening ""
                end
            elseif char == '%'
                actualCommentStartIdx = i; % Found the start of a real comment
                break; % Exit loop, comment found
            end
        end
        i = i + 1;
    end

    if actualCommentStartIdx ~= -1
        if actualCommentStartIdx == 1
            codeP = ''; % Line is entirely a comment
            commentP = trimmedLine;
        else
            codeP = strtrim(trimmedLine(1:actualCommentStartIdx-1));
            commentContent = strtrim(trimmedLine(actualCommentStartIdx+1:end));
            if isempty(commentContent) && actualCommentStartIdx == len % Handles "code%"
                commentP = '%';
            elseif isempty(commentContent) % Handles "code %  " (comment is just spaces)
                commentP = ' %';
            else
                commentP = [' % ', commentContent];
            end
        end
    end
end

function validateStylePreset(presetName)
if ~(ischar(presetName) || (isstring(presetName) && isscalar(presetName)))
    ME = MException('code_beautifier:InvalidStylePresetType', ...
        'StylePreset must be a character vector or a scalar string.');
    throwAsCaller(ME); 
end
presetName = char(presetName); 

if isempty(presetName) 
    return;
end

validPresetNames = {'Default', 'MathWorksStyle', 'CompactStyle'};
isActuallyValid = false;
for i = 1:length(validPresetNames)
    if strcmpi(presetName, validPresetNames{i}) 
        isActuallyValid = true;
        break;
    end
end

if ~isActuallyValid 
    ME = MException('code_beautifier:InvalidStylePreset', ...
        'Unknown StylePreset: "%s". Valid presets are ''Default'', ''MathWorksStyle'', ''CompactStyle''. Use an empty string for no preset.', presetName);
    throwAsCaller(ME);
end
end

function knownInfo = getKnownOptionsInfo(defaultSettings)
knownInfo = struct(); 
optionNames = fieldnames(defaultSettings); 
for i = 1:length(optionNames)
    optName = optionNames{i};
    if strcmpi(optName, 'StylePreset')
        continue; 
    end
    value = defaultSettings.(optName);
    if islogical(value)
        knownInfo.(optName).type = 'logical';
    elseif isnumeric(value)
        knownInfo.(optName).type = 'numeric';
        if strcmp(optName, 'MinBlankLinesBeforeBlock')
            knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x) && x >= 0 && x <= 2 && floor(x) == x;
            knownInfo.(optName).range = [0, 2]; 
        elseif any(strcmp(optName, {'IndentSize', 'ContinuationIndentOffset'}))
            knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x; 
        else
            knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x); 
        end
    elseif ischar(value) || isstring(value) 
        knownInfo.(optName).type = 'string';
        if strcmp(optName, 'OutputFormat')
            knownInfo.(optName).validator = @(x) (ischar(x) || (isstring(x) && isscalar(x))) && ismember(lower(char(x)), {'cell', 'char'});
            knownInfo.(optName).allowed = {'cell', 'char'}; 
        end
    end
end
end

% This function is not directly called in the provided code, but kept for completeness if used by a different version of parseConfigFile
function validateStylePresetConfig(presetName) %#ok<DEFNU>
if ~(ischar(presetName) || (isstring(presetName) && isscalar(presetName)))
    ME = MException('code_beautifier:InvalidStylePresetTypeInConfigFile', ...
        'StylePreset in config file must be a string.');
    throw(ME); 
end
presetNameStr = char(presetName);
if isempty(presetNameStr) 
    return;
end
validPresets = {'Default', 'MathWorksStyle', 'CompactStyle'}; 
if ~ismember(lower(presetNameStr), lower(validPresets)) 
    ME = MException('code_beautifier:InvalidStylePresetInConfigFile', ...
        'Unknown StylePreset "%s" in config file. Valid are ''Default'', ''MathWorksStyle'', ''CompactStyle''.', presetNameStr);
    throw(ME);
end
end


function parsedOptions = parseConfigFile(filePath, knownInfo)
parsedOptions = struct(); 
try
    fid = fopen(filePath, 'rt'); 
    if fid == -1 
        warning('code_beautifier:ConfigFileNotFound', ...
            'Configuration file .mbeautifyrc not found or cannot be opened.');
        return;
    end
    C = onCleanup(@() fclose(fid)); 

    lineNumber = 0;
    while ~feof(fid) 
        lineNumber = lineNumber + 1;
        line = strtrim(fgetl(fid)); 

        if isempty(line) || startsWith(line, '#')
            continue;
        end

        parts = regexp(line, '^\s*([^#=\s]+)\s*=\s*([^#]+?)\s*$', 'tokens');
        if isempty(parts) 
            warning('code_beautifier:InvalidLineInConfigFile', ...
                'Skipping invalid line %d in .mbeautifyrc: "%s". Line must be in "key = value" format.', lineNumber, line);
            continue;
        end

        key = strtrim(parts{1}{1}); 
        valueStr = strtrim(parts{1}{2}); 

        if strcmpi(key, 'StylePreset')
            if isempty(valueStr)
                warning('code_beautifier:EmptyStylePresetValueInConfigFile', ...
                    'Skipping StylePreset on line %d in .mbeautifyrc because its value is empty.', lineNumber);
                continue;
            end
            parsedOptions.ConfigFileStylePreset = valueStr; 
            continue; 
        end

        canonicalKey = '';
        knownOptionNames = fieldnames(knownInfo);
        for k_idx = 1:length(knownOptionNames)
            if strcmpi(key, knownOptionNames{k_idx}) 
                canonicalKey = knownOptionNames{k_idx};
                break;
            end
        end

        if isempty(canonicalKey) 
            warning('code_beautifier:UnknownConfigFileOption', ...
                'Skipping unknown option "%s" on line %d in .mbeautifyrc.', key, lineNumber);
            continue;
        end

        info = knownInfo.(canonicalKey); 
        try
            parsedValue = [];
            switch info.type 
                case 'logical'
                    if strcmpi(valueStr, 'true')
                        parsedValue = true;
                    elseif strcmpi(valueStr, 'false')
                        parsedValue = false;
                    else
                        error('Value must be "true" or "false" (case-insensitive).'); 
                    end
                case 'numeric'
                    parsedValue = str2double(valueStr); 
                    if isnan(parsedValue) 
                        error('Invalid numeric value: "%s".', valueStr);
                    end
                    if isfield(info, 'validator') && ~info.validator(parsedValue)
                        if isfield(info, 'range') 
                            error('Numeric value %g is out of allowed range [%g, %g] or not a valid integer.', parsedValue, info.range(1), info.range(2));
                        else
                            error('Numeric value %g is not valid for option "%s".', parsedValue, canonicalKey);
                        end
                    end
                case 'string'
                    parsedValue = valueStr; 
                    if isfield(info, 'validator') && ~info.validator(parsedValue)
                        if isfield(info, 'allowed') 
                            error('String value "%s" is not one of the allowed values: %s.', parsedValue, strjoin(info.allowed, ', '));
                        else
                            error('String value "%s" is not valid for option "%s".', parsedValue, canonicalKey);
                        end
                    end
                otherwise 
                    warning('code_beautifier:InternalParserError', ...
                        'Internal error: Unknown type "%s" for option "%s". Skipping.', info.type, canonicalKey);
                    continue;
            end
            parsedOptions.(canonicalKey) = parsedValue; 
        catch ME 
            warning('code_beautifier:InvalidValueInConfigFile', ...
                'Skipping option "%s" on line %d in .mbeautifyrc due to invalid value: %s (%s)', ...
                canonicalKey, lineNumber, valueStr, ME.message);
        end
    end
catch ME_file 
    warning('code_beautifier:ErrorReadingConfigFile', 'Error reading .mbeautifyrc: %s', ME_file.message);
end
end
