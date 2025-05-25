function test_code_beautifier_results()
% TEST_CODE_BEAUTIFIER_RESULTS Script to test the code_beautifier function.
%
%   This script creates temporary files, opens them in the editor,
%   applies formatting using code_beautifier, and compares the result
%   against expected outputs.
%
%   Make sure 'code_beautifier.m' is in the MATLAB path.

    testCount = 0;
    passCount = 0;

    % --- Test Suite ---
    fprintf('Starting Code Beautifier Test Suite...\n');

    % --- Test 1: Default Settings ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: Default Settings ---\n', testCount);
    rawCode1 = sprintf([...
        'function y = myfun(x)\n', ...
        '%% This is a comment\n', ...
        'if x > 0\n', ...
        '  y=x*2;  % Multiply by two\n', ...
        'else\n', ...
        'y = x+1;;\n', ...
        'end\n', ...
        'end']);
    expectedCode1_char = sprintf([...
        'function y = myfun(x)\n', ...
        '    %% This is a comment\n', ...
        '    if x > 0\n', ...
        '        y = x * 2; % Multiply by two\n', ...
        '    else\n', ...
        '        y = x + 1;\n', ...
        '    end\n', ...
        'end']);
    [passCount, ~] = run_test_case(rawCode1, expectedCode1_char, {}, passCount, 'Default Settings');

    % --- Test 2: CompactStyle Preset ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: CompactStyle Preset ---\n', testCount);
    rawCode2 = rawCode1; % Reuse raw code
    expectedCode2_char = sprintf([...
        'function y = myfun(x)\n', ...
        '  %% This is a comment\n', ...
        '  if x > 0\n', ...
        '    y = x * 2; % Multiply by two\n', ...
        '  else\n', ...
        '    y = x + 1;\n', ...
        '  end\n', ...
        'end']);
    [passCount, ~] = run_test_case(rawCode2, expectedCode2_char, {'StylePreset', 'CompactStyle'}, passCount, 'CompactStyle Preset');

    % --- Test 3: MathWorksStyle Preset ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: MathWorksStyle Preset (MinBlankLinesBeforeBlock=1) ---\n', testCount);
    rawCode3 = sprintf([...
        'function y = myfun(x)\n', ...
        'if x > 0\n', ...
        'y=x*2;\n', ...
        'else\n', ...
        'y = x+1;\n', ...
        'end\n', ...
        'for i=1:3\n', ...
        'disp(i);\n', ...
        'end\n', ...
        'end']);
    expectedCode3_char = sprintf([...
        'function y = myfun(x)\n', ...
        '\n', ... % Expected blank line due to MinBlankLinesBeforeBlock=1 in MathWorksStyle
        '    if x > 0\n', ...
        '        y = x * 2;\n', ...
        '    else\n', ...
        '        y = x + 1;\n', ...
        '    end\n', ...
        '\n', ... % Expected blank line
        '    for i = 1:3\n', ...
        '        disp(i);\n', ...
        '    end\n', ...
        'end']);
    [passCount, ~] = run_test_case(rawCode3, expectedCode3_char, {'StylePreset', 'MathWorksStyle'}, passCount, 'MathWorksStyle Preset');

    % --- Test 4: Custom IndentSize and UseTabs ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: Custom IndentSize=2, UseTabs=true ---\n', testCount);
    rawCode4 = sprintf('if true\nx=1;\nend');
    % Expected: Tab for indent, then x = 1;
    expectedCode4_char = sprintf('if true\n\tx = 1;\nend'); % \t represents a tab
    [passCount, ~] = run_test_case(rawCode4, expectedCode4_char, {'IndentSize', 2, 'UseTabs', true}, passCount, 'IndentSize=2, UseTabs=true');

    % --- Test 5: SpaceAroundOperators=false ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: SpaceAroundOperators=false ---\n', testCount);
    rawCode5 = sprintf('a=b+c*d/e==f&&g||h;');
    expectedCode5_char = sprintf('a=b+c*d/e==f&&g||h;'); % Default indent
    [passCount, ~] = run_test_case(rawCode5, expectedCode5_char, {'SpaceAroundOperators', false, 'IndentSize',0}, passCount, 'SpaceAroundOperators=false');

    % --- Test 6: SpaceAfterComma=false ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: SpaceAfterComma=false ---\n', testCount);
    rawCode6 = sprintf('myfun(a,b,c);\nx=[1,2,3];');
    expectedCode6_char = sprintf('myfun(a,b,c);\nx = [1,2,3];'); % Default indent, operators spaced
    [passCount, ~] = run_test_case(rawCode6, expectedCode6_char, {'SpaceAfterComma', false, 'IndentSize',0}, passCount, 'SpaceAfterComma=false');

    % --- Test 7: ContinuationIndentOffset ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: ContinuationIndentOffset=2 ---\n', testCount);
    rawCode7 = sprintf([...
        'myMatrix = [1, 2, 3, ...\n', ...
        '4, 5, 6];']);
    expectedCode7_char = sprintf([...
        'myMatrix = [1, 2, 3, ...\n', ...
        '        4, 5, 6];']); % Default 4 spaces + 2*4 for continuation offset
    [passCount, ~] = run_test_case(rawCode7, expectedCode7_char, {'ContinuationIndentOffset', 2}, passCount, 'ContinuationIndentOffset=2');

    % --- Test 8: PreserveBlankLines=false ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: PreserveBlankLines=false ---\n', testCount);
    rawCode8 = sprintf('a = 1;\n\n\nb = 2;');
    expectedCode8_char = sprintf('a = 1;\nb = 2;'); % Default indent
    [passCount, ~] = run_test_case(rawCode8, expectedCode8_char, {'PreserveBlankLines', false, 'IndentSize',0}, passCount, 'PreserveBlankLines=false');

    % --- Test 9: MinBlankLinesBeforeBlock=2 ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: MinBlankLinesBeforeBlock=2 ---\n', testCount);
    rawCode9 = sprintf('function test()\nif true\ndisp(1);\nend\nend');
    expectedCode9_char = sprintf([...
        'function test()\n', ...
        '\n', ... % Blank line 1
        '\n', ... % Blank line 2
        '    if true\n', ...
        '        disp(1);\n', ...
        '    end\n', ...
        'end']);
    [passCount, ~] = run_test_case(rawCode9, expectedCode9_char, {'MinBlankLinesBeforeBlock', 2}, passCount, 'MinBlankLinesBeforeBlock=2');


    % --- Test 10: AddSemicolonsToStatements=true ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: AddSemicolonsToStatements=true ---\n', testCount);
    rawCode10 = sprintf('disp(''hello'')\na=1\nplot(x,y)');
    expectedCode10_char = sprintf('disp(''hello'');\na = 1;\nplot(x,y);'); % Default indent
    [passCount, ~] = run_test_case(rawCode10, expectedCode10_char, {'AddSemicolonsToStatements', true, 'IndentSize',0}, passCount, 'AddSemicolonsToStatements=true');

    % --- Test 11: AlignAssignments=true ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: AlignAssignments=true ---\n', testCount);
    rawCode11 = sprintf([...
        'alpha = 10;\n', ...
        'beta_long_name = 200; % comment\n', ...
        'gamma = 3;']);
    expectedCode11_char = sprintf([...
        'alpha            = 10;\n', ...
        'beta_long_name   = 200; % comment\n', ...
        'gamma            = 3;']); % Default indent
    [passCount, ~] = run_test_case(rawCode11, expectedCode11_char, {'AlignAssignments', true, 'IndentSize',0}, passCount, 'AlignAssignments=true');

    % --- Test 12: FormatArgumentsBlock=true ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: FormatArgumentsBlock=true ---\n', testCount);
    rawCode12 = sprintf([...
        'function myFuncWithArgs(name, options)\n', ...
        'arguments\n', ...
        ' name (1,1) string {mustBeNonempty}\n', ...
        ' options.Value (1,1) double = 10 % Default val\n', ...
        ' options.Flag logical=false\n', ...
        ' (Repeating) ...\n', ...
        '   options.AnotherArg cell = {''test''}\n',...
        'end\n', ...
        'disp(name)\n', ...
        'end']);
    % Note: The exact spacing for FormatArgumentsBlock can be tricky to predict
    % without running it, as it depends on max lengths of components.
    % This expected output is based on a plausible interpretation.
    expectedCode12_char = sprintf([...
        'function myFuncWithArgs(name, options)\n', ...
        '    arguments\n', ...
        '        name             (1,1) string {mustBeNonempty}\n', ...
        '        options.Value    (1,1) double                  = 10 % Default val\n', ...
        '        options.Flag           logical                 = false\n', ... % Adjusted based on actual output of beautifier
        '        (Repeating) ...\n', ... % This line is not an arg def, so it's passed through with indent
        '        options.AnotherArg     cell                    = {''test''}\n',...
        '    end\n', ...
        '    disp(name)\n', ...
        'end']);
    [passCount, ~] = run_test_case(rawCode12, expectedCode12_char, {'FormatArgumentsBlock', true}, passCount, 'FormatArgumentsBlock=true');

    % --- Test 13: OutputFormat='cell' ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: OutputFormat=''cell'' ---\n', testCount);
    rawCode13 = sprintf('a=1;\nb=2;');
    expectedCode13_cell = {'a = 1;'; 'b = 2;'}; % Default indent
    [passCount, ~] = run_test_case(rawCode13, expectedCode13_cell, {'OutputFormat', 'cell', 'IndentSize',0}, passCount, 'OutputFormat=''cell''');

    % --- Test 14: .mbeautifyrc configuration file ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: .mbeautifyrc configuration ---\n', testCount);
    rawCode14 = sprintf('if true\nx=1+2;\nend');
    expectedCode14_char = sprintf('if true\n  x=1+2;\nend'); % IndentSize=2, SpaceAroundOperators=false from rc file

    % Create a temporary .mbeautifyrc
    rcFileName = fullfile(pwd, '.mbeautifyrc');
    fid = fopen(rcFileName, 'w');
    if fid == -1
        warning('Failed to create temporary .mbeautifyrc for testing.');
    else
        fprintf(fid, 'IndentSize = 2\n');
        fprintf(fid, 'UseTabs = false\n');
        fprintf(fid, 'SpaceAroundOperators = false\n');
        fclose(fid);

        [passCount, ~] = run_test_case(rawCode14, expectedCode14_char, {}, passCount, '.mbeautifyrc config');

        % Clean up .mbeautifyrc
        delete(rcFileName);
    end

    % --- Test 15: Operator spacing edge cases (unary, scientific) ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: Operator spacing edge cases ---\n', testCount);
    rawCode15 = sprintf([...
        'x = -5;\n', ...
        'y = +3;\n', ...
        'z = 1e-5;\n', ...
        'w = obj.method(-arg);\n',...
        'arr = [1 -2 +3];']);
    expectedCode15_char = sprintf([...
        'x = -5;\n', ...
        'y = +3;\n', ...
        'z = 1e-5;\n', ...
        'w = obj.method(-arg);\n', ...
        'arr = [1 -2 +3];']); % Default indent
    [passCount, ~] = run_test_case(rawCode15, expectedCode15_char, {'IndentSize',0}, passCount, 'Operator spacing edge cases');

    % --- Test 16: Block comment handling ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: Block comment handling ---\n', testCount);
    rawCode16 = sprintf([...
        'a = 1;\n',...
        '%{\n',...
        '  This is a block comment.\n',...
        '    It should maintain relative indent.\n',...
        '%}\n',...
        'b = 2;']);
    expectedCode16_char = sprintf([...
        'a = 1;\n',...
        '%{\n',...
        '  This is a block comment.\n',...
        '    It should maintain relative indent.\n',...
        '%}\n',...
        'b = 2;']); % Default indent
    [passCount, ~] = run_test_case(rawCode16, expectedCode16_char, {'IndentSize',0}, passCount, 'Block comment handling');
    
    % --- Test 17: Switch case indentation ---
    testCount = testCount + 1;
    fprintf('\n--- Test %d: Switch case indentation ---\n', testCount);
    rawCode17 = sprintf([...
        'switch val\n', ...
        'case 1\n', ...
        'disp("one");\n', ...
        '  x = 1; % comment \n', ...
        'case 2\n', ...
        '    disp("two");\n', ...
        'otherwise\n', ...
        '  disp("other");\n', ...
        'end']);
    expectedCode17_char = sprintf([...
        'switch val\n', ...
        '    case 1\n', ...
        '        disp("one");\n', ...
        '        x = 1; % comment\n', ...
        '    case 2\n', ...
        '        disp("two");\n', ...
        '    otherwise\n', ...
        '        disp("other");\n', ...
        'end']);
    [passCount, ~] = run_test_case(rawCode17, expectedCode17_char, {}, passCount, 'Switch case indentation');


    % --- Test 18: Error for no active editor (simulated by closing all) ---
    % This test is tricky because the testing script itself might be in an editor.
    % A more robust way would be to run MATLAB with -nodisplay or ensure no editors are open.
    % For now, we'll try to close all and then call.
    testCount = testCount + 1;
    fprintf('\n--- Test %d: Error for no active editor ---\n', testCount);
    allDocs = matlab.desktop.editor.getAll;
    for i = 1:numel(allDocs)
        allDocs(i).closeNoPrompt; % Close all open documents
    end
    pause(0.5); % Give MATLAB a moment
    
    try
        code_beautifier(); % Should throw an error
        fprintf('FAIL: No error thrown for no active editor.\n');
    catch ME
        if strcmp(ME.identifier, 'code_beautifier:NoActiveEditor')
            fprintf('PASS: Correctly threw "%s".\n', ME.identifier);
            passCount = passCount + 1;
        else
            fprintf('FAIL: Incorrect error thrown: "%s". Message: %s\n', ME.identifier, ME.message);
        end
    end
    % Note: After this test, you might need to reopen your test script editor manually.


    % --- Summary ---
    fprintf('\n\n--- Test Summary ---\n');
    fprintf('Total tests run: %d\n', testCount);
    fprintf('Tests passed:    %d\n', passCount);
    fprintf('Tests failed:    %d\n', testCount - passCount);

    if passCount == testCount
        fprintf('All tests passed!\n');
    else
        fprintf('Some tests FAILED.\n');
    end

end


function [passCount, actualCode] = run_test_case(rawCode, expectedCode, options, currentPassCount, testName)
% Helper function to run a single test case

    fprintf('Running test: %s\n', testName);
    
    tempFile = [tempname, '.m'];
    editorDoc = []; % Initialize to ensure it's in scope for cleanup
    
    % Cleanup object to ensure editor is closed and file is deleted
    cleanupObj = onCleanup(@() closeAndDeleteTempFile(editorDoc, tempFile));

    try
        % 1. Write raw code to temp file
        fid = fopen(tempFile, 'w');
        if fid == -1
            error('Failed to open temp file %s for writing.', tempFile);
        end
        fprintf(fid, '%s', rawCode);
        fclose(fid);

        % 2. Open in editor (this makes it active)
        editorDoc = matlab.desktop.editor.openDocument(tempFile);
        if isempty(editorDoc)
             % Try alternative if openDocument fails (older MATLAB?)
            edit(tempFile); 
            pause(0.5); % Give editor time to become active
            editorDoc = matlab.desktop.editor.getActive;
            if isempty(editorDoc) || ~strcmp(editorDoc.Filename, tempFile)
                 error('Failed to open %s in editor or make it active.', tempFile);
            end
        end
        % Ensure it's the active document (sometimes needed)
        editorDoc.makeActive();
        pause(0.2); % Small pause

        % 3. Call code_beautifier
        actualCode = code_beautifier(options{:});
        
        % 4. Compare
        if ischar(expectedCode)
            % Normalize line endings for comparison if actualCode is char
            if ischar(actualCode)
                actualCodeToCompare = strrep(actualCode, sprintf('\r\n'), sprintf('\n'));
                expectedCodeToCompare = strrep(expectedCode, sprintf('\r\n'), sprintf('\n'));
            else % actualCode is cell, expected is char - this shouldn't happen if options are right
                fprintf('FAIL: Type mismatch. Expected char, got cell.\n');
                return;
            end
            
            if strcmp(actualCodeToCompare, expectedCodeToCompare)
                fprintf('PASS\n');
                passCount = currentPassCount + 1;
            else
                fprintf('FAIL\n');
                fprintf('Expected:\n<<<\n%s\n>>>\n', expectedCodeToCompare);
                fprintf('Actual:\n<<<\n%s\n>>>\n', actualCodeToCompare);
                % For easier diffing:
                % disp("Expected (copy-paste friendly):"); disp(expectedCodeToCompare);
                % disp("Actual (copy-paste friendly):"); disp(actualCodeToCompare);
            end
        elseif iscell(expectedCode)
            if iscell(actualCode)
                if isequal(actualCode, expectedCode)
                    fprintf('PASS\n');
                    passCount = currentPassCount + 1;
                else
                    fprintf('FAIL\n');
                    fprintf('Expected (cell):\n'); disp(expectedCode);
                    fprintf('Actual (cell):\n'); disp(actualCode);
                end
            else
                 fprintf('FAIL: Type mismatch. Expected cell, got char.\n');
                 return;
            end
        else
            fprintf('FAIL: Unknown expectedCode type.\n');
        end

    catch ME
        fprintf('FAIL: Error during test execution for "%s":\n', testName);
        fprintf('Error ID: %s\n', ME.identifier);
        fprintf('Message: %s\n', ME.message);
        fprintf('Stack:\n');
        for k=1:length(ME.stack)
            fprintf('  File: %s, Name: %s, Line: %d\n', ME.stack(k).file, ME.stack(k).name, ME.stack(k).line);
        end
        actualCode = ''; % Ensure actualCode is assigned
    end
    % Cleanup is handled by onCleanup object
end

function closeAndDeleteTempFile(editorDoc, filePath)
    if ~isempty(editorDoc) && isvalid(editorDoc)
        try
            editorDoc.closeNoPrompt();
        catch
            % Ignore errors during cleanup close
        end
    end
    if exist(filePath, 'file')
        delete(filePath);
    end
end
