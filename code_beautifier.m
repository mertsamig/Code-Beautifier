function beautifulCode = code_beautifier(rawCode, varargin)
% code_beautifier Formats MATLAB code for better readability.
%
%   beautifulCode = code_beautifier(rawCode)
%   Formats the input MATLAB code (string, cell array of strings, or string array)
%   using default settings.
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
%   filePath = 'myScript.m'; % Path to an existing .m file
%   rawText = fileread(filePath);
%   formattedText = code_beautifier(rawText, 'OutputFormat', 'char', 'StylePreset', 'MathWorksStyle');
%   % To save back (BE CAREFUL - BACKUP YOUR ORIGINAL):
%   % fidOut = fopen('myScript_beautified.m', 'w');
%   % fprintf(fidOut, '%s', formattedText); % Note: no \n needed if formattedText has them
%   % fclose(fidOut);

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
        'AlignAssignments', false ...
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
        'AlignAssignments', false ...
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
        'AlignAssignments', false ...
    );

    % --- Determine Effective Defaults (Precedence: Function Defaults -> Config File -> Preset -> Direct Args) ---
    
    % 1. Start with hardcoded function defaults (via stylePresets.Default)
    effectiveDefaults = stylePresets.Default;
    
    % 2. Load and Overlay Config File Options
    knownOptionsInfo = getKnownOptionsInfo(stylePresets.Default); % Get type info for parsing
    configFilePath = fullfile(pwd, '.mbeautifyrc');
    configFileOptions = struct();
    if exist(configFilePath, 'file')
        configFileOptions = parseConfigFile(configFilePath, knownOptionsInfo);
        % Overlay configFileOptions onto effectiveDefaults
        fieldsToUpdate = fieldnames(configFileOptions);
        for k_f = 1:length(fieldsToUpdate)
            fieldName = fieldsToUpdate{k_f};
            if isfield(effectiveDefaults, fieldName) % Ensure it's a known option
                effectiveDefaults.(fieldName) = configFileOptions.(fieldName);
            end
        end
    end

    % 3. Determine and Overlay StylePreset Options
    %    The StylePreset can be from varargin (highest precedence for choosing the preset) or from config file.
    
    % Check varargin for 'StylePreset' first
    directArgPresetName = '';
    for k_v = 1:2:length(varargin)
        if strcmpi(varargin{k_v}, 'StylePreset') && k_v + 1 <= length(varargin)
            directArgPresetName = char(varargin{k_v+1});
            break;
        end
    end
    
    finalPresetName = '';
    if ~isempty(directArgPresetName)
        % Validate directArgPresetName. If invalid, validateStylePreset will error later.
        % For now, assume it might be valid to select it.
        finalPresetName = directArgPresetName;
    elseif isfield(configFileOptions, 'StylePreset') && ~isempty(configFileOptions.StylePreset)
        % Validate preset from config file
        try
            validateStylePreset(configFileOptions.StylePreset); % Ensure it's a known preset string
            finalPresetName = char(configFileOptions.StylePreset);
        catch ME
            warning('code_beautifier:InvalidStylePresetInConfigFile', ...
                    'Invalid StylePreset "%s" in .mbeautifyrc: %s. Ignoring this preset.', ...
                    configFileOptions.StylePreset, ME.message);
        end
    end

    if ~isempty(finalPresetName)
        % Check if this finalPresetName is actually valid before trying to access stylePresets.(finalPresetName)
        % The validateStylePreset function (used by inputParser later) will catch genuinely invalid names.
        % Here, we just need to ensure it's one of the defined preset structs.
        validPresetNames = fieldnames(stylePresets);
        isKnownPreset = false;
        for vp_idx = 1:length(validPresetNames)
            if strcmpi(finalPresetName, validPresetNames{vp_idx})
                finalPresetName = validPresetNames{vp_idx}; % Use the canonical casing
                isKnownPreset = true;
                break;
            end
        end

        if isKnownPreset
            presetSettingsToApply = stylePresets.(finalPresetName);
            fieldsToUpdate = fieldnames(presetSettingsToApply);
            for k_f = 1:length(fieldsToUpdate)
                fieldName = fieldsToUpdate{k_f};
                effectiveDefaults.(fieldName) = presetSettingsToApply.(fieldName);
            end
        elseif ~isempty(directArgPresetName) && strcmpi(directArgPresetName, finalPresetName)
            % If it came from direct arg and is not a known preset struct name (e.g. '  Default  ')
            % it will be caught by the main input parser's validateStylePreset.
            % We don't issue a warning here for that case.
        elseif ~isempty(finalPresetName) % Came from config or was an unknown preset string
             % Warning for unknown (but non-empty) preset names not from direct args already handled by validateStylePreset in config loading
             % or will be caught by the main parser.
        end
    end

    % --- Input Parsing Setup (using effectiveDefaults) ---
    p = inputParser;
    addRequired(p, 'rawCode', @(x) ischar(x) || iscellstr(x) || isstring(x));
    
    % Add parameters with their effective defaults and validation functions
    addParameter(p, 'StylePreset', effectiveDefaults.StylePreset, @validateStylePreset); % StylePreset default itself is '' or from config
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
    addParameter(p, 'OutputFormat', effectiveDefaults.OutputFormat, @(x) (ischar(x) || (isstring(x) && isscalar(x))) && ismember(lower(char(x)), {'cell', 'char'}));

    % 4. Parse Direct Arguments (varargin) which will override the effectiveDefaults
    parse(p, rawCode, varargin{:});
    options = p.Results;
    
    % Ensure StylePreset in options is the canonical one if it was matched
    if ~isempty(options.StylePreset)
        currentPresetVal = char(options.StylePreset);
        validPresetNames = fieldnames(stylePresets);
        for vp_idx = 1:length(validPresetNames)
            if strcmpi(currentPresetVal, validPresetNames{vp_idx})
                options.StylePreset = validPresetNames{vp_idx}; % Use canonical casing
                break;
            end
        end
    end

    if options.UseTabs
        indentChar = sprintf('\t'); % Use sprintf for tab character
        indentUnit = 1;
    else
        indentChar = ' ';
        indentUnit = options.IndentSize;
    end

    % Convert input to cell array of lines
    if ischar(rawCode)
        lines = strsplit(rawCode, {'\r\n', '\n', '\r'}, 'CollapseDelimiters', false)';
    elseif isstring(rawCode)
        if isscalar(rawCode)
             lines = strsplit(rawCode, {'\r\n', '\n', '\r'}, 'CollapseDelimiters', false)';
        else
            lines = cellstr(rawCode);
        end
    else % Assumed cellstr
        lines = rawCode;
    end

    % --- Keywords Definitions ---
    indentKeywords     = {'if', 'for', 'while', 'switch', 'try', 'parfor', 'function', 'classdef', 'properties', 'methods', 'events', 'arguments'};
    dedentKeywords     = {'end'};
    midBlockKeywords   = {'elseif', 'else', 'catch', 'case', 'otherwise'};
    allBlockCtrlKeywords = [indentKeywords, dedentKeywords, midBlockKeywords];
    firstWordPattern   = ['^\s*(', strjoin(allBlockCtrlKeywords, '|'), ')\b']; % \b for word boundary

    % --- Main Processing Loop ---
    indentLevel = 0;
    tempBeautifulLines = cell(size(lines)); % Pre-allocate for processed lines
    inBlockComment = false; % For %{ ... %}
    previousLineEndedWithContinuation = false;
    previousLineActualIndentStr = '';
    
    inSwitchBlockDepth = 0; 
    inCaseBody = false;     

    for i = 1:length(lines)
        originalLine = lines{i};
        trimmedOriginalLine = strtrim(originalLine);

        % --- Handle Block Comments %{ ... %} ---
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
            % Preserve original leading spaces within the block comment content itself, after base indent
            tempBeautifulLines{i} = [baseIndentStr, originalLine]; 
            previousLineEndedWithContinuation = false;
            continue;
        end

        % --- Handle Empty Lines (initially, will be refined later) ---
        if isempty(trimmedOriginalLine)
            tempBeautifulLines{i} = ''; 
            previousLineEndedWithContinuation = false;
            continue;
        end
        
        % --- Extract Code and Comment Parts ---
        [codePart, commentPart] = extractCodeAndCommentInternal(trimmedOriginalLine);
        
        % --- Determine First Word (Keyword) ---
        firstWordToken = regexp(codePart, firstWordPattern, 'tokens', 'once');
        if ~isempty(firstWordToken), firstWord = firstWordToken{1}; else, firstWord = ''; end

        % --- Line is Only a Comment ---
        if isempty(codePart) && ~isempty(commentPart)
            currentLineEffectiveIndentLevel = indentLevel;
            if inCaseBody % Comments inside case body also get case offset
                 currentLineEffectiveIndentLevel = currentLineEffectiveIndentLevel + 1;
            end
            currentIndentStr = repmat(indentChar, 1, currentLineEffectiveIndentLevel * indentUnit * (options.IndentSize > 0));
            
            if previousLineEndedWithContinuation % Comment continuing a code line
                currentIndentStr = [previousLineActualIndentStr, ...
                                    repmat(indentChar, 1, options.ContinuationIndentOffset * indentUnit * (options.IndentSize > 0))];
            end
            tempBeautifulLines{i} = regexprep([currentIndentStr, commentPart], '\s+$', '');
            previousLineEndedWithContinuation = false; 
            previousLineActualIndentStr = currentIndentStr; % Save indent of this comment line
            continue;
        end

        % --- Indentation Logic for Current Code Line ---
        currentLineEffectiveIndentLevel = indentLevel; % Default for code lines

        if ismember(firstWord, dedentKeywords) % Handles 'end'
            currentLineEffectiveIndentLevel = max(0, indentLevel - 1);
            % State changes like inSwitchBlockDepth-- and inCaseBody=false for 'end'
            % are handled in the "Update IndentLevel for NEXT line" section.
        elseif ismember(firstWord, midBlockKeywords) % Handles 'elseif', 'else', 'catch', 'case', 'otherwise'
            if ismember(firstWord, {'case', 'otherwise'})
                % 'case' and 'otherwise' keywords are indented to the current indentLevel (switch_content_level)
                % currentLineEffectiveIndentLevel remains 'indentLevel'
                inCaseBody = true;
            else % 'elseif', 'else', 'catch'
                % These keywords are indented one level less than the block they are in
                currentLineEffectiveIndentLevel = max(0, indentLevel - 1);
                inCaseBody = false; % Reset inCaseBody if inside if/elseif/else constructs
            end
        end
        
        % Additional indentation for statements within a 'case' or 'otherwise' body
        if inCaseBody && ~ismember(firstWord, allBlockCtrlKeywords) && ~isempty(firstWord)
             currentLineEffectiveIndentLevel = currentLineEffectiveIndentLevel + 1;
        end

        currentIndentStr = repmat(indentChar, 1, currentLineEffectiveIndentLevel * indentUnit * (options.IndentSize > 0));

        if previousLineEndedWithContinuation
            currentIndentStr = [previousLineActualIndentStr, ...
                                repmat(indentChar, 1, options.ContinuationIndentOffset * indentUnit * (options.IndentSize > 0))];
        end
        previousLineActualIndentStr = currentIndentStr; % Store for potential next continuation

        % --- Spacing and Semicolon Logic (Applied to codePart) ---
        processedCodePart = codePart;
        if ~isempty(processedCodePart)
            % Semicolon Management (before spacing operators, as it might change line end)
            if options.RemoveRedundantSemicolons
                processedCodePart = regexprep(processedCodePart, ';(\s*;)+', ';'); % ;;+ -> ;
                if strcmp(firstWord, 'end') && endsWith(strtrim(processedCodePart), ';')
                    tempTrimmed = strtrim(processedCodePart);
                    if ~ismember(tempTrimmed(end-1), {')', ']', '}'}) % Avoid end); -> end)
                         processedCodePart = strtrim(tempTrimmed(1:end-1));
                    end
                end
            end

            if options.AddSemicolonsToStatements
                isAssignment = ~isempty(regexp(processedCodePart, '(?<![=<>~.\s])=(?![=])', 'once')); % Avoid ==, <=, etc. and .=
                isKeywordLine = ~isempty(firstWord);
                endsWithContOrSemi = endsWith(strtrim(processedCodePart), '...') || endsWith(strtrim(processedCodePart), ';');
                % Heuristic: add semicolon if it looks like a function call or expression
                % and is not an assignment, keyword line, or already ends with ; or ...
                isFunctionCallLike = ~isempty(regexp(processedCodePart, '\w\s*\(.*\)', 'once')); % e.g. func()
                isSimpleExpression = ~isempty(regexp(processedCodePart, '\w', 'once')) && isempty(regexp(processedCodePart,'^\s*\w+\s*$', 'once')); % More than just a var

                if ~isAssignment && ~isKeywordLine && ~endsWithContOrSemi && (isFunctionCallLike || isSimpleExpression)
                    processedCodePart = [processedCodePart, ';'];
                end
            end

            % Operator Spacing
            if options.SpaceAroundOperators
                % Operators other than + and - (which need special handling)
                opListGeneral = { ...
                    '==', '~=', '<=', '>=', '&&', '||', ... % Comparison & Logical
                    '.*', './', '.\\', '.^', ...             % Element-wise arithmetic
                    '*', '/', '\\', '^', ...                 % Other arithmetic (binary context)
                    '=' ...                                  % Assignment (single)
                    };
                % Iterate to ensure longer ops are preferred (e.g. == over =)
                for op_idx = 1:length(opListGeneral)
                    op = opListGeneral{op_idx};
                    escaped_op = regexptranslate('escape', op);
                    % Pattern: non-whitespace, optional spaces, operator, optional spaces, non-whitespace
                    pat = ['(\S)\s*', escaped_op, '\s*(\S)']; 
                    rep = ['$1 ', op, ' $2'];
                    processedCodePart = regexprep(processedCodePart, pat, rep);
                end

                % Special handling for binary + and -
                % Pattern: operand1_suffix space(s) +/- space(s) operand2_prefix
                % Operand1_suffix: word character, ), ], or ' (transpose)
                % Operand2_prefix: word character, (, [, or . (dot for struct/method or start of element-wise op)
                s1 = '(\w|\)|\]|\'')'; % Capture group 1: char before operator
                s2 = '(\w|\(|\[|\.)'; % Capture group 3: char after operator
                pat_binary_plus_minus = [s1, '\s*([+\-])\s*', s2]; % Capture group 2: the operator +/-
                rep_binary_plus_minus = '$1 $2 $3'; % Add single spaces
                processedCodePart = regexprep(processedCodePart, pat_binary_plus_minus, rep_binary_plus_minus);
                
                % Unary plus/minus and scientific notation fixes
                % Fix 1: Remove space after certain preceding tokens if followed by unary +/-
                % Preceding tokens: assignment, delimiters, logical short-circuit ops
                unary_fix_class_1 = '[=\(\[\{,\s&|]'; % Note: removed * / \ ^ < > ~
                pat_unary_fix_1 = ['(', unary_fix_class_1, ')\s+([+\-])\s*(\w|[\.\(])'];
                processedCodePart = regexprep(processedCodePart, pat_unary_fix_1, '$1$2$3');
                
                % Fix 2: Remove space if unary +/- is at the beginning of the code part (after any indent)
                pat_unary_fix_2 = ['^([+\-])\s+(\w|[\.\(])'];
                processedCodePart = regexprep(processedCodePart, pat_unary_fix_2, '$1$2');
                
                % Fix scientific notation (e.g., 1 e - 5 -> 1e-5)
                % These should run after general spacing to catch cases like '1e - 5' or '1 e-5'
                processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*([+\-])\s*(\d+)', '$1e$2$3', 'ignorecase');
                processedCodePart = regexprep(processedCodePart, '(\d)\s*e\s*(\d+)', '$1e$2', 'ignorecase'); % For e.g. 1e10 (no sign)
            end

            if options.SpaceAfterComma
                processedCodePart = regexprep(processedCodePart, '\s*,\s*', ', ');
                processedCodePart = regexprep(processedCodePart, ', $', ','); 
            end
            
            processedCodePart = regexprep(processedCodePart, ';(\S)', '; $1'); % Space after semicolon separator: [1; 2]
        end

        % --- Construct the Beautiful Line ---
        % Conditional formatting to handle cases like "end % comment" vs "code; % comment"
        if isempty(strtrim(processedCodePart)) && ~isempty(commentPart) % Line was only comment (should be caught earlier)
            tempBeautifulLines{i} = regexprep([currentIndentStr, commentPart], '\s+$', '');
        elseif ~isempty(strtrim(processedCodePart)) && ~isempty(commentPart)
            tempBeautifulLines{i} = regexprep([currentIndentStr, strtrim(processedCodePart), commentPart], '\s+$', '');
        elseif ~isempty(strtrim(processedCodePart))
            tempBeautifulLines{i} = regexprep([currentIndentStr, strtrim(processedCodePart)], '\s+$', '');
        else % Both code and comment are empty after processing (e.g. was just whitespace or became empty)
            tempBeautifulLines{i} = ''; % Effectively an empty line
        end
        
        % --- Update IndentLevel for NEXT line ---
        if options.IndentSize > 0 % Only change indent level if indenting is active
            if ismember(firstWord, dedentKeywords) % 'end'
                current_indentLevel_before_dedent = indentLevel;
                indentLevel = max(0, indentLevel - 1);
                
                % If this 'end' keyword is potentially closing a switch block
                if inSwitchBlockDepth > 0
                    % This is a simplified model: assumes an 'end' might close the innermost switch.
                    % A more robust system would use a stack to match 'end' to 'switch'.
                    % If indentLevel actually decreased and was at a level consistent with switch content
                    if indentLevel < current_indentLevel_before_dedent 
                        % This 'end' caused a real dedent. Assume it closes the current switch block context.
                        inSwitchBlockDepth = max(0, inSwitchBlockDepth - 1);
                        if inSwitchBlockDepth == 0
                            inCaseBody = false; % Exited the outermost switch structure
                        end
                    end
                end
            elseif ismember(firstWord, midBlockKeywords) % 'elseif', 'else', 'catch', 'case', 'otherwise'
                if ismember(firstWord, {'case', 'otherwise'})
                    % indentLevel for lines after 'case'/'otherwise' does not change here.
                    % It's already at switch_content_level. Code inside the case
                    % is handled by the 'inCaseBody' flag causing further indentation
                    % of currentLineEffectiveIndentLevel.
                else % 'elseif', 'else', 'catch'
                    indentLevel = max(0, indentLevel - 1); % Dedent part for the keyword itself
                    indentLevel = indentLevel + 1;         % Indent part for the content of the block
                end
            elseif ismember(firstWord, indentKeywords) % 'if', 'for', 'switch', ...
                if strcmp(firstWord, 'switch')
                    inSwitchBlockDepth = inSwitchBlockDepth + 1;
                end
                indentLevel = indentLevel + 1;
            end
        end
        previousLineEndedWithContinuation = endsWith(strtrim(processedCodePart), '...');
    end

    % --- Post Processing: Blank Lines and MinBlankLinesBeforeBlock ---
    finalOutputLines = cell(1, length(tempBeautifulLines) + options.MinBlankLinesBeforeBlock * length(tempBeautifulLines)); % Overestimate
    finalLineCount = 0;
    lastMeaningfulLineWasBlank = true; % Treat start of file as preceded by blank

    for k = 1:length(tempBeautifulLines)
        currentLineContent = strtrim(tempBeautifulLines{k});
        isCurrentLineBlank = isempty(currentLineContent);

        % MinBlankLinesBeforeBlock logic
        % Only apply if not at the very start of the file (finalLineCount > 0)
        if options.MinBlankLinesBeforeBlock > 0 && ~isCurrentLineBlank && finalLineCount > 0
            % Check if current line starts a new block
            [codeP, ~] = extractCodeAndCommentInternal(currentLineContent); % Re-extract, simple
            firstWordToken = regexp(codeP, ['^\s*(', strjoin(indentKeywords, '|'), ')\b'], 'tokens', 'once');
            
            if ~isempty(firstWordToken) % It's a block-starting keyword
                blanksNeeded = options.MinBlankLinesBeforeBlock;
                % Count existing blanks before this line in finalOutputLines
                % (from last non-blank line in finalOutputLines)
                numExistingBlanks = 0;
                if finalLineCount > 0
                    for j = finalLineCount:-1:1
                        if isempty(strtrim(finalOutputLines{j}))
                            numExistingBlanks = numExistingBlanks + 1;
                        else
                            break; % Hit a non-blank line
                        end
                    end
                end
                
                for bl = 1:max(0, blanksNeeded - numExistingBlanks)
                    finalLineCount = finalLineCount + 1;
                    finalOutputLines{finalLineCount} = '';
                end
            end
        end

        % PreserveBlankLines logic
        if isCurrentLineBlank
            if options.PreserveBlankLines
                if ~lastMeaningfulLineWasBlank % Add this blank line
                    finalLineCount = finalLineCount + 1;
                    finalOutputLines{finalLineCount} = '';
                    lastMeaningfulLineWasBlank = true;
                end
                % else: current is blank, previous was also blank, so collapse
            else
                % Do not add blank line if not preserving
            end
        else % Current line is not blank
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

    % --- Output Formatting ---
    if strcmpi(options.OutputFormat, 'char')
        beautifulCode = strjoin(beautifulLines, sprintf('\n')); % Use sprintf for cross-platform newline
    else % 'cell'
        beautifulCode = beautifulLines;
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

    for i = 1:length(lines)
        currentLine = lines{i};
        trimmedLine = strtrim(currentLine);

        isAssignable = false;
        currentIndent = '';
        lhs = '';
        rhs = '';
        comment = '';
        equalsIndexInCode = -1;

        if isempty(trimmedLine) || startsWith(trimmedLine, '%') % Empty or full comment line
            % Process previous block and reset
        else
            currentIndent = regexp(currentLine, '^\s*', 'match', 'once');
            [codePart, commentPart] = extractCodeAndCommentInternal(trimmedLine); % commentPart includes leading ' %'
            
            % Check if it's a keyword line (not typically alignable)
            isKeywordLine = ~isempty(regexp(codePart, indentKeywordsPattern, 'once'));

            if ~isKeywordLine && ~endsWith(strtrim(codePart), '...') % Not a keyword and no line continuation
                % Find the first '=' that is not part of '==', '~=', '<=', '>=' etc.
                % This regex finds '=' not preceded/followed by another operator char forming a multi-char operator
                % It also tries to avoid finding '=' inside string literals by only checking up to first comment char.
                
                % Simpler approach: find all '=', then check context.
                % More robust: use a loop similar to extractCodeAndCommentInternal to find first non-string/non-comment '='
                
                tempCodeForEquals = codePart;
                inSingleQuote = false; inDoubleQuote = false;
                tempEqualsIndex = -1;
                for charIdx = 1:length(tempCodeForEquals)
                    char = tempCodeForEquals(charIdx);
                    if char == ''''
                        if charIdx+1 <= length(tempCodeForEquals) && tempCodeForEquals(charIdx+1) == '''' % Escaped
                            charIdx = charIdx + 1; % Skip next
                        elseif ~inDoubleQuote
                            inSingleQuote = ~inSingleQuote;
                        end
                    elseif char == '"'
                         if charIdx+1 <= length(tempCodeForEquals) && tempCodeForEquals(charIdx+1) == '"' % Escaped
                            charIdx = charIdx + 1; % Skip next
                         elseif ~inSingleQuote
                            inDoubleQuote = ~inDoubleQuote;
                         end
                    elseif char == '=' && ~inSingleQuote && ~inDoubleQuote
                        % Check if it's a standalone '='
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
                end
                equalsIndexInCode = tempEqualsIndex;

                if equalsIndexInCode > 0
                    isAssignable = true;
                    lhs = strtrim(codePart(1:equalsIndexInCode-1));
                    rhs = strtrim(codePart(equalsIndexInCode+1:end));
                    comment = commentPart; % Already has leading space if exists
                end
            end
        end

        if isAssignable
            if isempty(blockLinesIndices) || strcmp(currentIndent, blockLinesIndents{end})
                % Add to current block
                blockLinesIndices(end+1) = i;
                blockLinesContent{end+1} = struct('lhs', lhs, 'rhs', rhs, 'comment', comment, 'originalIndex', i);
                blockLinesIndents{end+1} = currentIndent;
                if options.UseTabs
                    % Approximate tab length for alignment; this is imperfect.
                    % A common convention is 8 spaces per tab for alignment calculations.
                    % Or, count tabs as 1 char and then adjust spaces.
                    % For simplicity, just count chars in LHS. True tab alignment is complex.
                    maxLhsLen = max(maxLhsLen, length(lhs));
                else
                    maxLhsLen = max(maxLhsLen, length(lhs));
                end
            else
                % Indentation changed, process previous block
                if ~isempty(blockLinesIndices)
                    lines = applyAlignmentToBlock(lines, blockLinesContent, blockLinesIndents{1}, maxLhsLen, options);
                end
                % Start new block
                blockLinesIndices = [i];
                blockLinesContent = {struct('lhs', lhs, 'rhs', rhs, 'comment', comment, 'originalIndex', i)};
                blockLinesIndents = {currentIndent};
                if options.UseTabs
                     maxLhsLen = length(lhs);
                else
                     maxLhsLen = length(lhs);
                end
            end
        else
            % Not assignable, process previous block and reset
            if ~isempty(blockLinesIndices)
                lines = applyAlignmentToBlock(lines, blockLinesContent, blockLinesIndents{1}, maxLhsLen, options);
            end
            blockLinesIndices = [];
            blockLinesContent = {};
            blockLinesIndents = {};
            maxLhsLen = 0;
        end
    end

    % Process any remaining block
    if ~isempty(blockLinesIndices)
        lines = applyAlignmentToBlock(lines, blockLinesContent, blockLinesIndents{1}, maxLhsLen, options);
    end
end

function lines = applyAlignmentToBlock(lines, blockContent, blockIndent, maxLhsLen, options)
    if length(blockContent) < 1, return; end % Don't align single lines by themselves (or make it an option?)
                                          % For now, let's align even single lines if they form a "block" of 1.
                                          % The prompt implies consecutive, so >1. Let's stick to >1 for now.
    if length(blockContent) < 2 && false % Set to true to only align blocks of 2 or more. Currently allows single "blocks".
        return;
    end


    for k = 1:length(blockContent)
        item = blockContent{k};
        idx = item.originalIndex;
        
        numSpacesBeforeEquals = maxLhsLen - length(item.lhs);
        spacesBeforeEqualsStr = repmat(' ', 1, numSpacesBeforeEquals);
        
        if options.SpaceAroundOperators
            % LHS<spaces_to_align> <space> = <space> RHS <comment>
            newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, ' = ', item.rhs, item.comment];
        else
            % LHS<spaces_to_align>=RHS <comment>
            newLine = [blockIndent, item.lhs, spacesBeforeEqualsStr, '=', item.rhs, item.comment];
        end
        lines{idx} = regexprep(newLine, '\s+$', ''); % Trim trailing whitespace
    end
end


% --- Helper function to extract code and comment parts ---
function [codeP, commentP] = extractCodeAndCommentInternal(lineStr)
    % This helper robustly separates the code part of a line from its trailing comment,
    % correctly handling '%' characters that might appear inside single or double quoted string literals.
    
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
        
        if char == '''' % Single quote
            if ~inDoubleQuoteString % Only process if not in a double-quoted string
                if i+1 <= len && trimmedLine(i+1) == '''' % Escaped single quote ''
                    i = i + 1; % Skip next quote
                else
                    inSingleQuoteString = ~inSingleQuoteString;
                end
            end
        elseif char == '"' % Double quote
            if ~inSingleQuoteString % Only process if not in a single-quoted string
                if i+1 <= len && trimmedLine(i+1) == '"' % Escaped double quote ""
                    i = i + 1; % Skip next quote
                else
                    inDoubleQuoteString = ~inDoubleQuoteString;
                end
            end
        elseif char == '%'
            if ~inSingleQuoteString && ~inDoubleQuoteString
                actualCommentStartIdx = i;
                break; % Found the actual comment start
            end
        end
        i = i + 1;
    end

    if actualCommentStartIdx ~= -1
        if actualCommentStartIdx == 1 % Line starts with comment (e.g., "% comment")
            codeP = ''; 
            commentP = trimmedLine; % The whole trimmed line is the comment
        else
            codeP = strtrim(trimmedLine(1:actualCommentStartIdx-1));
            commentContent = strtrim(trimmedLine(actualCommentStartIdx+1:end));
            if isempty(commentContent) && actualCommentStartIdx == len % trailing % like "code %"
                 commentP = '%'; % Just a '%' symbol as comment
            elseif isempty(commentContent) % trailing % with space like "code % "
                 commentP = ' %';
            else
                commentP = [' % ', commentContent]; % Standardize to " % content"
            end
        end
    % else: no comment found, or '%' is inside a string. codeP remains the whole trimmedLine.
    end
end

% --- Validation function for StylePreset ---
function validateStylePreset(presetName)
    if ~(ischar(presetName) || (isstring(presetName) && isscalar(presetName)))
        % This error should ideally be caught by inputParser's own type checks if we define type more strictly.
        % However, this custom validation also handles it.
        ME = MException('code_beautifier:InvalidStylePresetType', 'StylePreset must be a character vector or a scalar string.');
        throwAsCaller(ME); % Throw as if the main function threw it for better stack trace
    end
    presetName = char(presetName); % Convert to char for easier handling

    if isempty(presetName)
        return; % Empty string is valid (no preset chosen)
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

% --- Helper function to get known option types and validators ---
function knownInfo = getKnownOptionsInfo(defaultSettings)
    knownInfo = struct();
    optionNames = fieldnames(defaultSettings);
    for i = 1:length(optionNames)
        optName = optionNames{i};
        value = defaultSettings.(optName);
        if islogical(value)
            knownInfo.(optName).type = 'logical';
        elseif isnumeric(value)
            knownInfo.(optName).type = 'numeric';
            if strcmp(optName, 'MinBlankLinesBeforeBlock')
                knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x) && x >= 0 && x <= 2 && floor(x) == x;
                knownInfo.(optName).range = [0, 2]; % For error messages
            elseif any(strcmp(optName, {'IndentSize', 'ContinuationIndentOffset'}))
                knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x;
            else % For other numerics if any, basic check
                knownInfo.(optName).validator = @(x) isnumeric(x) && isscalar(x);
            end
        elseif ischar(value) || isstring(value)
            knownInfo.(optName).type = 'string';
            if strcmp(optName, 'OutputFormat')
                knownInfo.(optName).validator = @(x) (ischar(x) || (isstring(x) && isscalar(x))) && ismember(lower(char(x)), {'cell', 'char'});
                knownInfo.(optName).allowed = {'cell', 'char'}; % For error messages
            elseif strcmp(optName, 'StylePreset') % StylePreset itself can be in config
                knownInfo.(optName).validator = @validateStylePresetConfig; % Slightly different validation for config context
            end
        end
    end
end

function validateStylePresetConfig(presetName)
    % Validation for StylePreset when read from config file (allows empty or valid name)
    if ~(ischar(presetName) || (isstring(presetName) && isscalar(presetName)))
        ME = MException('code_beautifier:InvalidStylePresetTypeInConfigFile', 'StylePreset in config file must be a string.');
        throw(ME); % Throw directly, parseConfigFile will catch and warn
    end
    presetNameStr = char(presetName);
    if isempty(presetNameStr)
        return; % Empty is fine
    end
    validPresets = {'Default', 'MathWorksStyle', 'CompactStyle'};
    if ~ismember(lower(presetNameStr), lower(validPresets))
        ME = MException('code_beautifier:InvalidStylePresetInConfigFile', ...
                        'Unknown StylePreset "%s" in config file. Valid are ''Default'', ''MathWorksStyle'', ''CompactStyle''.', presetNameStr);
        throw(ME);
    end
end


% --- Helper function to parse .mbeautifyrc config file ---
function parsedOptions = parseConfigFile(filePath, knownInfo)
    parsedOptions = struct();
    try
        fid = fopen(filePath, 'rt');
        if fid == -1
            warning('code_beautifier:ConfigFileNotFound', 'Configuration file .mbeautifyrc not found or cannot be opened.');
            return;
        end
        C = onCleanup(@() fclose(fid)); % Ensure file is closed
        
        lineNumber = 0;
        while ~feof(fid)
            lineNumber = lineNumber + 1;
            line = strtrim(fgetl(fid));
            
            if isempty(line) || startsWith(line, '#')
                continue; % Skip empty lines and comments
            end
            
            parts = regexp(line, '^\s*([^#=\s]+)\s*=\s*([^#]+?)\s*$', 'tokens');
            if isempty(parts)
                warning('code_beautifier:InvalidLineInConfigFile', ...
                        'Skipping invalid line %d in .mbeautifyrc: "%s". Line must be in "key = value" format.', lineNumber, line);
                continue;
            end
            
            key = strtrim(parts{1}{1});
            valueStr = strtrim(parts{1}{2});
            
            % Find canonical key name (case-insensitive match)
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
                        parsedValue = valueStr; % Already a string
                        if strcmp(canonicalKey, 'StylePreset') % Special handling for StylePreset string
                           validateStylePresetConfig(parsedValue); % Use the config-specific validator
                           % Ensure canonical casing for preset names if matched
                            validPresetNamesInner = {'Default', 'MathWorksStyle', 'CompactStyle'};
                            for vpi = 1:length(validPresetNamesInner)
                                if strcmpi(parsedValue, validPresetNamesInner{vpi})
                                    parsedValue = validPresetNamesInner{vpi};
                                    break;
                                end
                            end
                        elseif isfield(info, 'validator') && ~info.validator(parsedValue)
                             if isfield(info, 'allowed')
                                error('String value "%s" is not one of the allowed values: %s.', parsedValue, strjoin(info.allowed, ', '));
                            else
                                error('String value "%s" is not valid for option "%s".', parsedValue, canonicalKey);
                            end
                        end
                    otherwise
                        warning('code_beautifier:InternalParserError', 'Internal error: Unknown type "%s" for option "%s". Skipping.', info.type, canonicalKey);
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
