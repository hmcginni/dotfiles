function mdls = bdlist
% BDLIST - list all loaded Simulink models.

    if ~isSimulinkStarted
        disp('No Simulink models open.');
        return
    end

    mdlList = find_system('type', 'block_diagram');
    
    if isempty(mdlList)
        disp('No Simulink models open.')
        return
    end
    
    % ──────────────────────────────────────────────────────────
    % Display list of models unless called with an output variable
    
    if nargout == 0
        disp('Open Simulink models:');
        disp(mdlList);
    elseif nargout == 1
        mdls = mdlList;
    end
    
end
