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
    'OutputFormat', 'char' ... 
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
    'OutputFormat', 'char' ... % Added OutputFormat for consistency, will be overridden by Default if not specified
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
    'OutputFormat', 'char' ... % Added OutputFormat for consistency
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
tempBeautifulLines = cell(size(lines)); 
inBlockComment = false; 
previousLineEndedWithContinuation = false; 
previousLineActualIndentStr = ''; 

inSwitchBlockDepth = 0; 
inCaseBody = false;     

for i = 1:length(lines) 
    originalLine = lines{i}; 
    trimmedOriginalLine = strtrim(originalLine); 

    if startsWith(trimmedOriginalLine, '%{') 
        inBlockComment = true;
        baseIndentStr = repmat(indentChar, 1, indentLevel * indentUnit * (options.IndentSize > 0) );
        tempBeautifulLines{i} = [baseIndentStr, trimmedOriginalLine]; 
        previousLineEndedWithContinuation = false;
        continue; 
    elseif inBlockComment 
        baseIndentStr = repmat(indentChar, 1, indentLevel * indentUnit * (options.IndentSize > 0) );
        if endsWith(trimmedOriginalLine, '%}') 
            inBlockComment = false;
        end
        tempBeautifulLines{i} = [baseIndentStr, originalLine];
        previousLineEndedWithContinuation = false;
        continue; 
    end

    if isempty(trimmedOriginalLine)
        tempBeautifulLines{i} = '';
        previousLineEndedWithContinuation = false;
        continue; 
    end

    [codePart, commentPart] = extractCodeAndCommentInternal(trimmedOriginalLine);
    % fprintf('DEBUG_EXTRACTION: line %d, trimmedOriginalLine="%s", codePart="%s", commentPart="%s"\n', i, trimmedOriginalLine, codePart, commentPart);

    tempWords = strsplit(strtrim(codePart)); 
    if ~isempty(tempWords)
        potentialFirstWord = tempWords{1};
        potentialFirstWord = regexprep(potentialFirstWord, '[^a-zA-Z_].*$', ''); 

        if ismember(potentialFirstWord, allBlockCtrlKeywords)
            firstWord = potentialFirstWord;
        else
            firstWord = ''; 
        end
    else
        firstWord = ''; 
    end
    % fprintf('DEBUG_POST_FIRSTWORD_CALC: line %d, firstWord="%s", codePart="%s"\n', i, firstWord, codePart);

    if isempty(codePart) && ~isempty(commentPart) 
        currentLineEffectiveIndentLevel = indentLevel;
        if inCaseBody
            currentLineEffectiveIndentLevel = currentLineEffectiveIndentLevel + 1;
        end
        currentIndentStr = repmat(indentChar, 1, currentLineEffectiveIndentLevel * indentUnit * (options.IndentSize > 0));

        if previousLineEndedWithContinuation
            currentIndentStr = [previousLineActualIndentStr, ... 
                repmat(indentChar, 1, options.ContinuationIndentOffset * indentUnit * (options.IndentSize > 0))]; 
        end
        % fprintf('DEBUG_COMMENT_INDENT: line %d, currentIndentStr length=%d, level=%d, unit=%d, charVal=%d\n', i, length(currentIndentStr), currentLineEffectiveIndentLevel, indentUnit, uint8(indentChar(1)));
        tempBeautifulLines{i} = regexprep([currentIndentStr, commentPart], '\s+$', ''); 
        previousLineEndedWithContinuation = false;
        previousLineActualIndentStr = currentIndentStr; 
        continue; 
    end

    currentLineEffectiveIndentLevel = indentLevel; 

    if ismember(firstWord, dedentKeywords) 
        currentLineEffectiveIndentLevel = max(0, indentLevel - 1); 
    elseif ismember(firstWord, midBlockKeywords) 
        if ismember(firstWord, {'case', 'otherwise'})
            inCaseBody = true; 
        else 
            currentLineEffectiveIndentLevel = max(0, indentLevel - 1);
            inCaseBody = false; 
        end
    end

    if inCaseBody && ~ismember(firstWord, allBlockCtrlKeywords) && ~isempty(firstWord) 
        currentLineEffectiveIndentLevel = currentLineEffectiveIndentLevel + 1; 
    end

    currentIndentStr = repmat(indentChar, 1, currentLineEffectiveIndentLevel * indentUnit * (options.IndentSize > 0));

    if previousLineEndedWithContinuation
        currentIndentStr = [previousLineActualIndentStr, ... 
            repmat(indentChar, 1, options.ContinuationIndentOffset * indentUnit * (options.IndentSize > 0))]; 
    end
    previousLineActualIndentStr = currentIndentStr; 

    processedCodePart = codePart; 
    if ~isempty(processedCodePart)
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
            isAssignment = ~isempty(regexp(processedCodePart, '(?<![=<>~.\s])=(?![=])', 'once')); 
            isKeywordLine = ~isempty(firstWord); 
            endsWithContOrSemi = endsWith(strtrim(processedCodePart), '...') || endsWith(strtrim(processedCodePart), ';'); 

            isFunctionCallLike = ~isempty(regexp(processedCodePart, '\w\s*\(.*\)', 'once')); 
            isSimpleExpression = ~isempty(regexp(processedCodePart, '\w', 'once')) && ... 
                isempty(regexp(processedCodePart,'^\s*\w+\s*$', 'once')); 

            if ~isAssignment && ~isKeywordLine && ~endsWithContOrSemi && (isFunctionCallLike || isSimpleExpression)
                processedCodePart = [processedCodePart, ';']; 
            end
        end

        if options.SpaceAroundOperators
            % fprintf('DEBUG: Entering SpaceAroundOperators block. options.SpaceAroundOperators = %d\n', options.SpaceAroundOperators);
            % fprintf('DEBUG: SO: processedCodePart (before any regex): "%s"\n', processedCodePart);
            
            % fprintf('DEBUG: Processing specific relational/equality ops.\n');
            relationalOps = {'==', '~=', '<=', '>=', '<', '>'}; 
            % lhs_capture = '([\w\)\]''\.]|\d+\.\d*|\.\d+)'; % Defined but not used
            % rhs_capture = '([\w\(\[''"]|\d+\.\d*|\.\d+)'; % Defined but not used
            for k_rel = 1:length(relationalOps)
                op_rel = relationalOps{k_rel};
                escaped_op_rel = regexptranslate('escape', op_rel);
                pat_rel_robust = ['(\S)\s*', escaped_op_rel, '\s*(\S)']; 
                rep_rel = ['$1 ', op_rel, ' $2'];
                
                % REMOVED specific DEBUG_GT_REGEXPREP_INPUT block
                
                old_processedCodePart_rel = processedCodePart; 
                processedCodePart = regexprep(processedCodePart, pat_rel_robust, rep_rel);
                if ~strcmp(old_processedCodePart_rel, processedCodePart)
                    % fprintf('DEBUG_REL_OP: Line %d, Op "%s", Before: "%s", After: "%s"\n', i, op_rel, old_processedCodePart_rel, processedCodePart);
                end
            end
            % fprintf('DEBUG: processedCodePart after specific relational/equality ops: "%s"\n', processedCodePart);
            
            func_def_pat_v3 = '^(\s*function\s+[^=]+?)\s*=\s*(.+)$'; 
            func_def_rep_v3 = '$1 = $2'; 
            
            old_processedCodePart_func_def = processedCodePart; 
            processedCodePart = regexprep(processedCodePart, func_def_pat_v3, func_def_rep_v3, 'once');
            
            if ~strcmp(old_processedCodePart_func_def, processedCodePart)
                % fprintf('DEBUG_FUNC_DEF_EQ: Line %d, After (V3 pattern): "%s"\n', i, processedCodePart);
            else
                if startsWith(strtrim(processedCodePart), 'function')
                     % fprintf('DEBUG_FUNC_DEF_EQ: Line %d, No change with V3 pattern for: "%s"\n', i, old_processedCodePart_func_def);
                end
            end
            
            opListGeneral = { ...
                '&&', '||', ... 
                '.*', './', '.\\', '.^', ...             
                '*', '/', '\\', '^', ...                 
                '=' ...                                  
                };
            % fprintf('DEBUG: opListGeneral (updated) = {');
            % for k_op_debug = 1:length(opListGeneral) 
            %     fprintf(' ''%s''', opListGeneral{k_op_debug});
            % end
            % fprintf(' }\n');
            
            for op_idx = 1:length(opListGeneral)
                op = opListGeneral{op_idx};
                escaped_op = regexptranslate('escape', op); 
                pat = ['(\S)\s*', escaped_op, '\s*(\S)'];
                rep = ['$1 ', op, ' $2']; 
                processedCodePart = regexprep(processedCodePart, pat, rep);
            end
            % fprintf('DEBUG_SO_GENERAL: line %d, after general ops: "%s"\n', i, processedCodePart);

            s1 = '(\w|\)|\]|\'')'; 
            s2 = '(\w|\(|\[|\.)'; 
            pat_binary_plus_minus = [s1, '\s*([+\-])\s*', s2]; 
            rep_binary_plus_minus = '$1 $2 $3'; 
            processedCodePart = regexprep(processedCodePart, pat_binary_plus_minus, rep_binary_plus_minus);
            % fprintf('DEBUG_SO_PLUSMINUS: line %d, after +/- ops: "%s"\n', i, processedCodePart);

            unary_fix_class_1 = '[=\(\[\{,\s&|]';
            pat_unary_fix_1 = ['(', unary_fix_class_1, ')\s+([+\-])\s*(\w|[\.\(])'];
            processedCodePart = regexprep(processedCodePart, pat_unary_fix_1, '$1$2$3');
            % fprintf('DEBUG_SO_UNARY1: line %d, after unary_fix_1: "%s"\n', i, processedCodePart);

            pat_unary_fix_2 = ['^([+\-])\s+(\w|[\.\(])'];
            processedCodePart = regexprep(processedCodePart, pat_unary_fix_2, '$1$2');
            % fprintf('DEBUG_SO_UNARY2: line %d, after unary_fix_2: "%s"\n', i, processedCodePart);

            processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*([+\-])\s*(\d+)', '$1e$2$3', 'ignorecase'); 
            processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*(\d+)', '$1e$2', 'ignorecase'); 
            % fprintf('DEBUG_SO_SCIENTIFIC: line %d, after scientific_fix: "%s"\n', i, processedCodePart);
        end

        if options.SpaceAfterComma
            processedCodePart = regexprep(processedCodePart, '\s*,\s*', ', '); 
            processedCodePart = regexprep(processedCodePart, ', $', ','); 
            % fprintf('DEBUG_SO_COMMA: line %d, after comma_space: "%s"\n', i, processedCodePart);
        end

        processedCodePart = regexprep(processedCodePart, ';(\S)', '; $1');
    end

    if isempty(strtrim(processedCodePart)) && ~isempty(commentPart) 
        tempBeautifulLines{i} = regexprep([currentIndentStr, commentPart], '\s+$', '');
    elseif ~isempty(strtrim(processedCodePart)) && ~isempty(commentPart) 
        tempBeautifulLines{i} = regexprep([currentIndentStr, strtrim(processedCodePart), commentPart], '\s+$', '');
    elseif ~isempty(strtrim(processedCodePart)) 
        tempBeautifulLines{i} = regexprep([currentIndentStr, strtrim(processedCodePart)], '\s+$', '');
    else 
        tempBeautifulLines{i} = '';
    end

    % fprintf('DEBUG_PRE_INDENT_UPDATE: line %d, firstWord="%s", indentLevel before update (for next line)=%d\n', i, firstWord, indentLevel);
    if options.IndentSize > 0 
        if ismember(firstWord, dedentKeywords) 
            current_indentLevel_before_dedent = indentLevel;
            indentLevel = max(0, indentLevel - 1); 

            if inSwitchBlockDepth > 0 
                if indentLevel < current_indentLevel_before_dedent 
                    inSwitchBlockDepth = max(0, inSwitchBlockDepth - 1);
                    if inSwitchBlockDepth == 0 
                        inCaseBody = false; 
                    end
                end
            end
        elseif ismember(firstWord, midBlockKeywords) 
            if ismember(firstWord, {'case', 'otherwise'})
                % indentLevel does not change here for 'case'/'otherwise'
            else 
                indentLevel = max(0, indentLevel - 1); 
                indentLevel = indentLevel + 1;         
            end
        elseif ismember(firstWord, indentKeywords) 
            if strcmp(firstWord, 'switch')
                inSwitchBlockDepth = inSwitchBlockDepth + 1; 
            end
            indentLevel = indentLevel + 1; 
        end
    end
    previousLineEndedWithContinuation = endsWith(strtrim(processedCodePart), '...'); 
end

% --- Post Processing: Blank Lines and MinBlankLinesBeforeBlock ---
finalOutputLines = cell(1, length(tempBeautifulLines) + options.MinBlankLinesBeforeBlock * length(tempBeautifulLines)); 
finalLineCount = 0;
lastMeaningfulLineWasBlank = true; 

for k = 1:length(tempBeautifulLines) 
    currentLineContent = strtrim(tempBeautifulLines{k}); 
    isCurrentLineBlank = isempty(currentLineContent); 

    if options.MinBlankLinesBeforeBlock > 0 && ~isCurrentLineBlank && finalLineCount > 0
        [codeP_for_blank_check, ~] = extractCodeAndCommentInternal(currentLineContent); 
        firstWordToken_for_blank_check = regexp(codeP_for_blank_check, ['^\s*(', strjoin(indentKeywords, '|'), ')\b'], 'tokens', 'once');

        if ~isempty(firstWordToken_for_blank_check) 
            blanksNeeded = options.MinBlankLinesBeforeBlock;
            numExistingBlanks = 0;
            if finalLineCount > 0
                for j = finalLineCount:-1:1 
                    if isempty(strtrim(finalOutputLines{j}))
                        numExistingBlanks = numExistingBlanks + 1;
                    else
                        break; 
                    end
                end
            end

            for bl = 1:max(0, blanksNeeded - numExistingBlanks)
                finalLineCount = finalLineCount + 1;
                finalOutputLines{finalLineCount} = '';
            end
        end
    end

    if isCurrentLineBlank
        if options.PreserveBlankLines 
            if ~lastMeaningfulLineWasBlank 
                finalLineCount = finalLineCount + 1;
                finalOutputLines{finalLineCount} = ''; 
                lastMeaningfulLineWasBlank = true;
            end
        end
    else 
        finalLineCount = finalLineCount + 1;
        finalOutputLines{finalLineCount} = tempBeautifulLines{k}; 
        lastMeaningfulLineWasBlank = false;
    end
end
beautifulLines = finalOutputLines(1:finalLineCount)'; 

% --- Optional: Align Assignments ---
if options.AlignAssignments && ~isempty(beautifulLines)
    beautifulLines = alignAssignmentBlocksInternal(beautifulLines, options);
end

% --- Optional: Format Arguments Blocks ---
% FIXED: Added call to format arguments blocks if option is enabled
if options.FormatArgumentsBlock && ~isempty(beautifulLines)
    beautifulLines = applyArgumentsBlockFormatting(beautifulLines, options, indentChar, indentUnit);
end


% --- Output Formatting ---
if strcmpi(options.OutputFormat, 'char')
    beautifulCode = strjoin(beautifulLines, sprintf('\n')); 
else 
    beautifulCode = beautifulLines; 
end
end

% --- Helper function to apply 'arguments' block formatting (NEW) ---
function lines = applyArgumentsBlockFormatting(lines, options, indentChar, indentUnit)
    if isempty(lines) || ~options.FormatArgumentsBlock % Redundant check, but safe
        return; 
    end

    outputLines = lines; % Work on a copy (or directly on lines if careful)
    
    idx = 1;
    while idx <= length(outputLines)
        currentLine = outputLines{idx};
        % Use extractCodeAndCommentInternal to robustly find the first word
        [codePartCurrent, ~] = extractCodeAndCommentInternal(strtrim(currentLine));
        
        firstWordCurrent = '';
        tokensCurrent = regexp(codePartCurrent, '^\s*(\w+)', 'tokens', 'once');
        if ~isempty(tokensCurrent)
            firstWordCurrent = tokensCurrent{1};
        end

        if strcmp(firstWordCurrent, 'arguments')
            % Potential start of an arguments block
            argBlockContentLinesInput = {}; % Cell array to hold lines for formatArgumentsBlockInternal
            
            % Indices for lines that form the content of the arguments block
            contentLineIndices = []; 
            
            nestingLevel = 1; % Start with nesting level 1 for the 'arguments' keyword
            foundEnd = false;
            
            % Scan from the line *after* 'arguments'
            for j = (idx + 1) : length(outputLines)
                lineJ = outputLines{j};
                [codePartJ, ~] = extractCodeAndCommentInternal(strtrim(lineJ));
                firstWordJ = '';
                tokensJ = regexp(codePartJ, '^\s*(\w+)', 'tokens', 'once');
                if ~isempty(tokensJ)
                    firstWordJ = tokensJ{1};
                end

                % This simple model assumes 'end' directly closes 'arguments'.
                % It doesn't handle complex nesting of other 'end'-using blocks
                % *inside* an arguments definition line (which is rare/invalid).
                if strcmp(firstWordJ, 'arguments')
                    % This would be invalid MATLAB (nested arguments blocks)
                    % Or, it could be a comment 'arguments % arguments'
                    % For simplicity, we assume valid MATLAB structure.
                    % If truly nested, this logic might break.
                    nestingLevel = nestingLevel + 1; 
                elseif strcmp(firstWordJ, 'end')
                    nestingLevel = nestingLevel - 1;
                    if nestingLevel == 0
                        % Found the matching 'end' for our 'arguments' block.
                        % Content lines are from (idx + 1) to (j - 1).
                        if (j-1) >= (idx+1) % Check if there are any content lines
                            contentLineIndices = (idx+1):(j-1);
                            argBlockContentLinesInput = outputLines(contentLineIndices);
                        else
                            argBlockContentLinesInput = {}; % Empty arguments block
                        end
                        
                        if ~isempty(argBlockContentLinesInput) || true % Process even if empty to handle structure
                            formattedContent = formatArgumentsBlockInternal(argBlockContentLinesInput, options, indentChar, indentUnit);
                            
                            % Replace in outputLines
                            for k_block = 1:length(formattedContent)
                                outputLines{contentLineIndices(k_block)} = formattedContent{k_block};
                            end
                        end
                        
                        idx = j; % Continue scanning from this 'end' line
                        foundEnd = true;
                        break; % Exit inner loop (j), 'arguments' block processed
                    end
                end
                % If not 'arguments' or 'end', it's a content line (or comment)
                % to be collected if we were doing it differently, but here we just
                % identify the block boundaries and extract lines by slice.
            end % end inner loop (j)
            
            if ~foundEnd
                % No matching 'end' found for 'arguments' block.
                % The 'arguments' line at outputLines{idx} is left as is.
                % The outer loop will continue from idx + 1.
                % warning('code_beautifier:ArgumentsBlockNotClosed', 'An "arguments" block starting on line %d (approx) was not properly closed with "end". Formatting for this block skipped.', idx);
            end
        end
        idx = idx + 1; % Move to next line in outer loop
    end
    lines = outputLines; % Return the modified lines
end


% --- Helper function to format 'arguments' blocks ---
function formattedBlockLines = formatArgumentsBlockInternal(blockLines, options, indentChar, indentUnit) %#ok<INUSD>
% indentChar and indentUnit are passed for potential future use, not currently used for overall block indent.

if isempty(blockLines)
    formattedBlockLines = {};
    return;
end

parsedArgs = struct('name', {}, 'sizeClass', {}, 'validators', {}, ...
    'defaultValue', {}, 'hasDefaultValue', {}, 'comment', {}, 'originalLine', {}, ... % FIXED: Added hasDefaultValue
    'isCommentOnly', {}, 'indentStr', {});

namePattern = '^\s*([a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*)';
sizeClassPattern = '\s*((?:\([^\)]*?\))?\s*[a-zA-Z_]\w*)';
validatorsPattern = '\s*(\{[^\}]*\})';
% defaultValuePattern = '\s*=\s*(.+)'; % Original: greedy, requires something after =
defaultValuePattern = '\s*=\s*(.*)';   % Changed to .*: allows "name =" (empty default) to be captured

for i = 1:length(blockLines)
    line = blockLines{i};
    parsedArgs(i).originalLine = line;
    parsedArgs(i).isCommentOnly = false;
    parsedArgs(i).hasDefaultValue = false; % FIXED: Initialize hasDefaultValue

    leadingWhitespace = regexp(line, '^\s*', 'match', 'once');
    parsedArgs(i).indentStr = leadingWhitespace;

    trimmedLine = strtrim(line);

    if isempty(trimmedLine)
        parsedArgs(i).name = ''; 
        parsedArgs(i).comment = ''; 
        continue;
    end

    if startsWith(trimmedLine, '%')
        parsedArgs(i).isCommentOnly = true;
        parsedArgs(i).comment = trimmedLine; 
        continue;
    end

    [codePart, commentPart] = extractCodeAndCommentInternal(trimmedLine); 
    parsedArgs(i).comment = commentPart;
    currentCode = codePart; 

    nameMatch = regexp(currentCode, namePattern, 'tokens', 'once');
    if ~isempty(nameMatch)
        parsedArgs(i).name = strtrim(nameMatch{1});
        currentCode = currentCode(length(nameMatch{1})+1:end);
    else
        parsedArgs(i).name = ''; 
    end
    currentCode = strtrim(currentCode); 

    sizeClassMatch = regexp(currentCode, ['^', sizeClassPattern], 'tokens', 'once');
    if ~isempty(sizeClassMatch)
        parsedArgs(i).sizeClass = strtrim(sizeClassMatch{1});
        currentCode = regexprep(currentCode, ['^', regexptranslate('escape', parsedArgs(i).sizeClass)], '', 'once');
    else
        parsedArgs(i).sizeClass = '';
    end
    currentCode = strtrim(currentCode);

    validatorsMatch = regexp(currentCode, ['^', validatorsPattern], 'tokens', 'once');
    if ~isempty(validatorsMatch)
        parsedArgs(i).validators = strtrim(validatorsMatch{1});
        currentCode = regexprep(currentCode, ['^', regexptranslate('escape', parsedArgs(i).validators)], '', 'once');
    else
        parsedArgs(i).validators = '';
    end
    currentCode = strtrim(currentCode);

    % FIXED: Refined default value parsing
    if startsWith(currentCode, '=')
        parsedArgs(i).hasDefaultValue = true; % Mark that '=' token was present
        defaultMatch = regexp(currentCode, defaultValuePattern, 'tokens', 'once');
        % defaultMatch will be non-empty if pattern matches.
        % defaultMatch{1} can be an empty char if "name =" due to (.*)
        if ~isempty(defaultMatch) 
            parsedArgs(i).defaultValue = strtrim(defaultMatch{1});
        else
            % This case should ideally not be reached if startsWith '=' and pattern is '=\s*(.*)'
            % unless currentCode is just '=' and regex engine behaves unexpectedly.
            % For safety, ensure defaultValue is set if hasDefaultValue is true.
            parsedArgs(i).defaultValue = ''; 
        end
    else
        % No default value part, .hasDefaultValue remains false, .defaultValue can be empty.
        parsedArgs(i).defaultValue = ''; 
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
    if ~isempty(parsedArgs(i).sizeClass)
        maxSizeClassLen = max(maxSizeClassLen, length(parsedArgs(i).sizeClass));
    end
    if ~isempty(parsedArgs(i).validators)
        maxValidatorsLen = max(maxValidatorsLen, length(parsedArgs(i).validators));
    end
end

formattedBlockLines = cell(size(blockLines));
for i = 1:length(parsedArgs)
    if parsedArgs(i).isCommentOnly
        formattedBlockLines{i} = [parsedArgs(i).indentStr, parsedArgs(i).comment];
        continue;
    end
    if isempty(parsedArgs(i).name) && isempty(parsedArgs(i).comment)
        formattedBlockLines{i} = parsedArgs(i).indentStr; 
        continue;
    end

    lineParts = {};
    lineParts{end+1} = parsedArgs(i).indentStr;

    nameStr = parsedArgs(i).name;
    namePadding = maxNameLen - length(nameStr);
    lineParts{end+1} = [nameStr, repmat(' ', 1, namePadding)];

    if ~isempty(parsedArgs(i).sizeClass)
        sizeClassStr = parsedArgs(i).sizeClass;
        sizeClassPadding = maxSizeClassLen - length(sizeClassStr);
        lineParts{end+1} = [' ', sizeClassStr, repmat(' ', 1, sizeClassPadding)];
    elseif maxSizeClassLen > 0 
        lineParts{end+1} = repmat(' ', 1, maxSizeClassLen + 1); 
    end

    if ~isempty(parsedArgs(i).validators)
        validatorsStr = parsedArgs(i).validators;
        validatorsPadding = maxValidatorsLen - length(validatorsStr);
        lineParts{end+1} = [' ', validatorsStr, repmat(' ', 1, validatorsPadding)];
    elseif maxValidatorsLen > 0 
        lineParts{end+1} = repmat(' ', 1, maxValidatorsLen + 1); 
    end

    % FIXED: Use hasDefaultValue flag for reconstruction
    if parsedArgs(i).hasDefaultValue
        defaultStr = parsedArgs(i).defaultValue; % This can be an empty char for "name ="
        if options.SpaceAroundOperators
            lineParts{end+1} = [' = ', defaultStr];
        else
            lineParts{end+1} = ['=', defaultStr];
        end
    end

    fullLine = strjoin(lineParts, '');
    if ~all(isspace(fullLine)) 
        fullLine = regexprep(fullLine, '\s+$', '');
    end

    if ~isempty(parsedArgs(i).comment)
        if ~isempty(strtrim(fullLine)) 
            fullLine = [fullLine, parsedArgs(i).comment]; 
        else 
            fullLine = [fullLine, strtrim(parsedArgs(i).comment)]; 
        end
    end

    formattedBlockLines{i} = fullLine; 
end
end

% --- Helper function to align assignment blocks ---
function lines = alignAssignmentBlocksInternal(lines, options)
if isempty(lines), return; end

blockLinesIndices = [];      
blockLinesContent = {};      
blockLinesIndents = {};      
maxLhsLen = 0;               

indentKeywordsPattern = ['^\s*(if|for|while|switch|try|parfor|function|classdef|properties|methods|events|arguments)\b'];

resetBlockState = @() deal([], {}, {}, 0); 

for i = 1:length(lines) 
    currentLine = lines{i};
    trimmedLine = strtrim(currentLine); 
    currentIndent = regexp(currentLine, '^\s*', 'match', 'once'); 

    if isempty(trimmedLine)
        if ~isempty(blockLinesIndices) 
            lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options); 
        end
        [blockLinesIndices, blockLinesContent, blockLinesIndents, maxLhsLen] = resetBlockState(); 
        continue; 
    end

    isFullCommentLine = startsWith(trimmedLine, '%'); 
    isAssignable = false; 
    lhs = ''; rhs = ''; commentPartForAssignment = ''; equalsIndexInCode = -1; 

    if ~isFullCommentLine
        [codePart, commentPartExtracted] = extractCodeAndCommentInternal(trimmedLine); 
        isKeywordLine = ~isempty(regexp(codePart, indentKeywordsPattern, 'once')); 

        if ~isKeywordLine && ~endsWith(strtrim(codePart), '...')
            tempCodeForEquals = codePart;
            inSingleQuote = false; inDoubleQuote = false; 
            tempEqualsIndex = -1; 
            charIdx = 1; % Initialize charIdx for the while loop
            while charIdx <= length(tempCodeForEquals) 
                char = tempCodeForEquals(charIdx);
                if char == '''' 
                    if charIdx+1 <= length(tempCodeForEquals) && tempCodeForEquals(charIdx+1) == '''' 
                        charIdx = charIdx + 1; 
                    elseif ~inDoubleQuote 
                        inSingleQuote = ~inSingleQuote;
                    end
                elseif char == '"' 
                    if charIdx+1 <= length(tempCodeForEquals) && tempCodeForEquals(charIdx+1) == '"' 
                        charIdx = charIdx + 1; 
                    elseif ~inSingleQuote 
                        inDoubleQuote = ~inDoubleQuote;
                    end
                elseif char == '=' && ~inSingleQuote && ~inDoubleQuote 
                    isComparison = false;
                    if charIdx > 1 && ismember(tempCodeForEquals(charIdx-1), {'=', '~', '<', '>'}) 
                        isComparison = true;
                    end
                    if charIdx < length(tempCodeForEquals) && tempCodeForEquals(charIdx+1) == '=' 
                        isComparison = true;
                    end
                    if ~isComparison
                        tempEqualsIndex = charIdx; 
                        break;
                    end
                end
                charIdx = charIdx + 1; % Increment charIdx
            end
            equalsIndexInCode = tempEqualsIndex;

            if equalsIndexInCode > 0 
                isAssignable = true;
                lhs = strtrim(codePart(1:equalsIndexInCode-1)); 
                rhs = strtrim(codePart(equalsIndexInCode+1:end)); 
                commentPartForAssignment = commentPartExtracted; 
            end
        end
    end

    if isAssignable
        if isempty(blockLinesIndices) || strcmp(currentIndent, blockLinesIndents{end})
            blockLinesIndices(end+1) = i; 
            blockLinesContent{end+1} = struct('type', 'assignment', ... 
                'lhs', lhs, 'rhs', rhs, ...
                'comment', commentPartForAssignment, ...
                'originalIndex', i, 'indentStr', currentIndent);
            blockLinesIndents{end+1} = currentIndent; 
            maxLhsLen = max(maxLhsLen, length(lhs));
        else 
            if ~isempty(blockLinesIndices) 
                lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options);
            end
            [blockLinesIndices, blockLinesContent, blockLinesIndents, maxLhsLen] = resetBlockState(); 
            blockLinesIndices(end+1) = i;
            blockLinesContent{end+1} = struct('type', 'assignment', ...
                'lhs', lhs, 'rhs', rhs, ...
                'comment', commentPartForAssignment, ...
                'originalIndex', i, 'indentStr', currentIndent);
            blockLinesIndents{end+1} = currentIndent;
            maxLhsLen = max(maxLhsLen, length(lhs));
        end
    elseif isFullCommentLine
        if ~isempty(blockLinesIndices) && strcmp(currentIndent, blockLinesIndents{end})
            blockLinesIndices(end+1) = i;
            blockLinesContent{end+1} = struct('type', 'comment', ...
                'lineValue', lines{i}, ... 
                'originalIndex', i, 'indentStr', currentIndent);
        else 
            if ~isempty(blockLinesIndices) 
                lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options);
            end
            [blockLinesIndices, blockLinesContent, blockLinesIndents, maxLhsLen] = resetBlockState(); 
        end
    else 
        if ~isempty(blockLinesIndices) 
            lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options);
        end
        [blockLinesIndices, blockLinesContent, blockLinesIndents, maxLhsLen] = resetBlockState(); 
    end
end

if ~isempty(blockLinesIndices)
    lines = applyAlignmentToBlock(lines, blockLinesContent, maxLhsLen, options);
end
end

function lines = applyAlignmentToBlock(lines, blockContent, maxLhsLen, options)
if length(blockContent) < 1, return; end 

for k = 1:length(blockContent) 
    item = blockContent{k};
    idx = item.originalIndex; 

    if strcmp(item.type, 'assignment') 
        blockIndent = item.indentStr; 

        numSpacesBeforeEquals = maxLhsLen - length(item.lhs);
        spacesBeforeEqualsStr = repmat(' ', 1, numSpacesBeforeEquals); 

        if options.SpaceAroundOperators 
            newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, ' = ', item.rhs, item.comment];
        else
            newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, '=', item.rhs, item.comment];
        end
        lines{idx} = regexprep(newLine, '\s+$', ''); 
    elseif strcmp(item.type, 'comment')
        % Comments are preserved, no action needed here.
    end
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

    if char == '''' 
        if ~inDoubleQuoteString 
            if i+1 <= len && trimmedLine(i+1) == '''' 
                i = i + 1; 
            else
                inSingleQuoteString = ~inSingleQuoteString; 
            end
        end
    elseif char == '"' 
        if ~inSingleQuoteString 
            if i+1 <= len && trimmedLine(i+1) == '"' 
                i = i + 1; 
            else
                inDoubleQuoteString = ~inDoubleQuoteString; 
            end
        end
    elseif char == '%' 
        if ~inSingleQuoteString && ~inDoubleQuoteString 
            actualCommentStartIdx = i; 
            break; 
        end
    end
    i = i + 1;
end

if actualCommentStartIdx ~= -1 
    if actualCommentStartIdx == 1 
        codeP = ''; 
        commentP = trimmedLine; 
    else
        codeP = strtrim(trimmedLine(1:actualCommentStartIdx-1));
        commentContent = strtrim(trimmedLine(actualCommentStartIdx+1:end));
        if isempty(commentContent) && actualCommentStartIdx == len 
            commentP = '%'; 
        elseif isempty(commentContent) 
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
