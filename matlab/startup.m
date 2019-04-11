%% Compact output format

format compact

%% Remove search box from GUI

try
    
    jDesk = com.mathworks.mde.desk.MLDesktop.getInstance;
    jPanel = jDesk.getMainFrame.getQuickAccessBar.getComponent.getParent.getParent;
    nComponents = jPanel.getComponentCount;
    removeMask = false(1,nComponents);
    
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
    
    jPanel.remove(removeMask);
    clear jDesk jPanel nComponents removeMask ctr
    
catch e
end
