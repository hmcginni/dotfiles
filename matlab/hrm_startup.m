%% Add utils to path

addpath('~/src/utils/functions');

%% Compact output format

format compact

%% Remove search box from GUI

isDesktopSession = usejava('desktop');

if isDesktopSession
    try
        jDesk = com.mathworks.mde.desk.MLDesktop.getInstance;
        jPanel = jDesk.getMainFrame.getQuickAccessBar.getComponent.getParent.getParent;
        nComponents = jPanel.getComponentCount;
    
        ctr = 0;
    
        while ctr < nComponents
        
            item = jPanel.getComponent(ctr);
            if contains(char(item),'DocCenterBrowserSearchBox')
                jPanel.remove(item);
                ctr = ctr - 1;
                nComponents = jPanel.getComponentCount;
            end
            ctr = ctr + 1;
        
        end
    
    catch e
        disp('Unable to remove the search box from the MATLAB desktop.');
    end
end


%% Customize the preferred MATLAB text editor
% 1. For -nodesktop mode, use Emacs
% 2. For -desktop mode, use MATLAB Editor

if isDesktopSession
    % Switch to MATLAB Editor as the text editor
    try
        com.mathworks.services.Prefs.setBooleanPref('EditorBuiltinEditor', true);
        com.mathworks.services.Prefs.setStringPref('EditorOtherEditor', '');
    catch e
        disp('Unable to set MATLAB Editor as the text editor.');
    end
    
else
    % Switch to Emacs as text editor
    try
        com.mathworks.services.Prefs.setBooleanPref('EditorBuiltinEditor', false);
        com.mathworks.services.Prefs.setStringPref('EditorOtherEditor', 'emacsclient -a "emacs"');
    catch e
        fprintf('\nUnable to set Emacs as the text editor.\n%s\n', e.message);
    end
    
end

clear
