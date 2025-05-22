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
        'AddSemicolonsToStatements', false ...
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
        'AddSemicolonsToStatements', false ...
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
        'AddSemicolonsToStatements', false ...
    );

    % --- Input Parsing Setup ---
    p = inputParser;
    addRequired(p, 'rawCode', @(x) ischar(x) || iscellstr(x) || isstring(x));
    addParameter(p, 'StylePreset', '', @(x) ischar(x) || isstring(x)); % Accept char or string for preset name
    addParameter(p, 'IndentSize', stylePresets.Default.IndentSize, @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x);
    addParameter(p, 'UseTabs', stylePresets.Default.UseTabs, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'SpaceAroundOperators', stylePresets.Default.SpaceAroundOperators, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'SpaceAfterComma', stylePresets.Default.SpaceAfterComma, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'ContinuationIndentOffset', stylePresets.Default.ContinuationIndentOffset, @(x) isnumeric(x) && isscalar(x) && x >= 0 && floor(x) == x);
    addParameter(p, 'PreserveBlankLines', stylePresets.Default.PreserveBlankLines, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'MinBlankLinesBeforeBlock', stylePresets.Default.MinBlankLinesBeforeBlock, @(x) isnumeric(x) && isscalar(x) && x >= 0 && x <=2 && floor(x) == x);
    addParameter(p, 'RemoveRedundantSemicolons', stylePresets.Default.RemoveRedundantSemicolons, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'AddSemicolonsToStatements', stylePresets.Default.AddSemicolonsToStatements, @(x) islogical(x) && isscalar(x));
    addParameter(p, 'OutputFormat', 'cell', @(x) (ischar(x) || (isstring(x) && isscalar(x))) && ismember(lower(char(x)), {'cell', 'char'}));

    % --- Preset Application Logic ---
    % First, parse just to find the StylePreset if provided
    tempP = inputParser;
    addParameter(tempP, 'StylePreset', '', @(x) ischar(x) || isstring(x));
    parse(tempP, varargin{:}); % Only parse varargin for StylePreset
    providedPresetName = char(tempP.Results.StylePreset); % Ensure it's char

    presetVarargin = {};
    if ~isempty(providedPresetName)
        if strcmpi(providedPresetName, 'Default') % Handle 'Default' explicitly
            presetSettings = stylePresets.Default;
        elseif strcmpi(providedPresetName, 'MathWorksStyle')
            presetSettings = stylePresets.MathWorksStyle;
        elseif strcmpi(providedPresetName, 'CompactStyle')
            presetSettings = stylePresets.CompactStyle;
        else
            % If an invalid preset name is given, it will be caught by the main parser later
            % if we add validation for StylePreset there. For now, we just don't apply a preset.
            % Or, we could error here: error('code_beautifier:InvalidStylePreset', 'Unknown StylePreset: %s', providedPresetName);
            presetSettings = struct(); % Empty struct, no settings
        end
        
        if ~isempty(fieldnames(presetSettings))
            presetFields = fieldnames(presetSettings);
            presetVarargin = cell(1, length(presetFields) * 2);
            for k = 1:length(presetFields)
                presetVarargin{2*k-1} = presetFields{k};
                presetVarargin{2*k} = presetSettings.(presetFields{k});
            end
        end
    end
    
    % Combine preset options (which go first) with user-provided varargin (which can override)
    combinedVarargin = [presetVarargin, varargin];

    % Now parse with the combined arguments
    parse(p, rawCode, combinedVarargin{:});
    options = p.Results;
    
    % If StylePreset was specified, but it was an unknown one, options.StylePreset will hold it.
    % We might want to add validatestring to the main parser's StylePreset definition
    % to formally reject unknown presets.
    % For now, if StylePreset in options is not empty and not one of the known ones,
    % it means an invalid one was passed and no preset settings were applied for it.
    % We could add a warning or error here if desired.
    % Example: if ~isempty(options.StylePreset) && ~isfield(stylePresets, options.StylePreset)
    %    warning('code_beautifier:UnknownStylePreset', 'Unknown StylePreset "%s" was specified and ignored.', options.StylePreset);
    % end


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


    % --- Output Formatting ---
    if strcmpi(options.OutputFormat, 'char')
        beautifulCode = strjoin(beautifulLines, sprintf('\n')); % Use sprintf for cross-platform newline
    else % 'cell'
        beautifulCode = beautifulLines;
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
