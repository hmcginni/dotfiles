%% Compact output format

format compact

%% Remove search box from GUI

isDesktopSession = usejava('desktop');

if isDesktopSession
    %
    % 1. Disable the search box
    % 2. Set the MATLAB Editor as the text editor
    %
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
        
        clear 
        
    catch e
    end
    
    com.mathworks.services.Prefs.setBooleanPref('EditorBuiltinEditor', true);
    com.mathworks.services.Prefs.setStringPref('EditorOtherEditor', '')

else
    %
    % Switch to Emacs as text editor
    %
    com.mathworks.services.Prefs.setBooleanPref('EditorBuiltinEditor', false);
    com.mathworks.services.Prefs.setStringPref('EditorOtherEditor', ...
                                               'emacsclient -a "emacs"')
    
end