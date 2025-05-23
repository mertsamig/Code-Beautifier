function beautify_active_matlab_script(varargin)
% BEAUTIFY_ACTIVE_MATLAB_SCRIPT Beautifies the currently active script in the MATLAB editor.
%
%   This function uses `matlab.desktop.editor` API to find the active
%   script, then applies `code_beautifier` to its content. The editor
%   content is then updated with the beautified code.
%
%   Pass Name-Value pair arguments for `code_beautifier` directly to this
%   function. For example:
%       beautify_active_matlab_script('StylePreset', 'CompactStyle');
%       beautify_active_matlab_script('FormatArgumentsBlock', true, 'IndentSize', 2);
%
%   Note: This function requires a MATLAB version that supports the
%   `matlab.desktop.editor` API (typically R2011b or later, with more
%   robust features in R2019b+).

    % 1. Check for Editor API Availability
    if ~exist('matlab.desktop.editor.getActiveDocument', 'file') % More specific check
        if exist('matlab.desktop.editor.getActive', 'file')
             % Older version might have getActive but not getActiveDocument immediately
        else
            errordlg(['This beautifier integration requires a newer version of MATLAB ' ...
                      'with the matlab.desktop.editor API (e.g., R2011b+).'], ...
                      'Beautifier: API Not Found', 'modal');
            return;
        end
    end

    % 2. Get Active Document
    try
        doc = matlab.desktop.editor.getActiveDocument(); % Preferred for R2011b+
    catch ME_getActiveDoc
        % Fallback for slightly older versions if getActiveDocument isn't available
        % but getActive might be (though less likely to be an issue if first check passes)
        try
            doc = matlab.desktop.editor.getActive();
        catch ME_getActive
            errordlg(['Failed to get active editor document. ' ME_getActive.message], ...
                     'Beautifier: Editor Error', 'modal');
            return;
        end
    end

    if isempty(doc)
        disp('No active script found in the MATLAB editor.');
        % Optionally, provide a non-modal message or just return silently
        % uialert(figure('Visible','off'), 'No active script found.', 'Beautifier Info', 'Icon','info', 'Modal',false, 'AutoClose',true);
        return;
    end

    % 3. Check Document Type and Path
    filePath = doc.Filename;

    if isempty(filePath)
        % Attempt to beautify unsaved "Untitled" documents based on current text
        if startsWith(doc.Name, 'Untitled') % doc.Name is like 'Untitled.m', 'Untitled2.m'
             if ~doc.Modified % Check if it's truly empty and unmodified
                 if isempty(strtrim(doc.Text))
                    disp('Active document is an empty "Untitled" script. Nothing to beautify.');
                    return;
                 end
             end
            disp(['Beautifying unsaved document: ' doc.Name]);
            currentText = doc.Text;
            try
                % Pass 'OutputFormat','char' to ensure string output for editor
                beautifiedText = code_beautifier(currentText, 'OutputFormat', 'char', varargin{:});
                if ~strcmp(currentText, beautifiedText)
                    doc.Text = beautifiedText;
                    disp([doc.Name ' has been beautified and updated in the editor. Please save it manually.']);
                else
                    disp([doc.Name ' is already beautified according to the current settings.']);
                end
            catch ME
                disp(['Error beautifying ' doc.Name ': ' ME.message]);
                errordlg(['Error during beautification of ' doc.Name ': ' ME.message], 'Beautifier Error', 'modal');
            end
            return; % Stop here for unsaved files
        else
            disp('The active document is unsaved or its path is not available. Please save it first.');
            return;
        end
    end

    if ~endsWith(lower(filePath), '.m')
        disp(['The active document "' filePath '" is not a MATLAB script or function (.m file).']);
        return;
    end

    % 4. Call `code_beautifier` using file path (preferred for saved files)
    %    or current text to ensure any unsaved changes are captured.
    %    Using doc.Text is safer as it reflects the current editor buffer.
    currentText = doc.Text;
    try
        disp(['Beautifying ' filePath '...']);
        % Pass 'OutputFormat','char' to ensure string output for editor
        beautifiedText = code_beautifier(currentText, 'OutputFormat', 'char', varargin{:});
    catch ME
        disp(['Error beautifying script "' filePath '": ' ME.message]);
        errordlg(['Error during beautification of "' filePath '": ' ME.message], 'Beautifier Error', 'modal');
        return;
    end

    % 5. Update Editor Content
    if ~strcmp(currentText, beautifiedText) % Compare with original text from editor
        doc.Text = beautifiedText;
        disp([filePath ' has been beautified and updated in the editor.']);
        
        % Optional: Auto-save (could be a parameter to this wrapper function)
        % For now, prompt the user or leave saving to them.
        % Example:
        % choice = questdlg(['Save changes to ' doc.Name '?'], 'Beautifier', 'Save', 'Don''t Save', 'Don''t Save');
        % if strcmp(choice, 'Save')
        %    doc.save();
        %    disp([filePath ' saved.']);
        % end
        
        % To simply mark it as modified so user can save:
        % (Setting doc.Text should already mark it as modified)

    else
        disp([filePath ' is already beautified according to the current settings. No changes made.']);
    end

end
