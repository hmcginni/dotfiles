%% Compact output format

format compact

%% Remove search box from GUI

try
	jDesk = com.mathworks.mde.desk.MLDesktop.getInstance;
	jPanel = jDesk.getMainFrame.getQuickAccessBar.getComponent.getParent.getParent;
	jSearchBox = jPanel.getComponent(jPanel.getComponentCount-2);
	
	if contains(char(jSearchBox),'DocCenterBrowserSearchBox')
		jPanel.remove(jSearchBox);
	end
	
	clear jDesk jPanel jSearchBox

catch e
end

%% Snag licenses


% check Simulink Stateflow Simulink_Test