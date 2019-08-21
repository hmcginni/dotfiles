function mnotify(title, msg)
    % 
    % MNOTIFY - Use `notify-send` to display message to user
    % 
    %   Usage:
    %     mnotify([title,] message)
    % 
    
    %% Parse inputs
    %
    
    if nargin == 1
        msg = title;
        title = 'MATLAB';
    end
    
    
    %% Set `notify-send` args
    %
    
    [status,~] = system('which notify-send');
    existNotifySend = ~status;
    icon = find_matlab_icon();
    
    if exist(icon, 'file')
        pngArgs = sprintf('-i %s', icon);
    else
        pngArgs = '';
    end
    
    numWords = numel(regexp(msg, '\w*[ .]'));
    timeout = num2str(200*numWords + 4000);
    args = sprintf('-t %s %s', timeout, pngArgs);
    
    
    %% Display Notification
    %
    
    if existNotifySend
        execStr = sprintf('notify-send %s "%s" "%s" &', args, title, msg);
        system(execStr);
    else
        fprintf('\n\t*** %s %s ***\n\n', title, msg);
    end
    
end



function icon = find_matlab_icon()
    %
    % FIND_MATLAB_ICON - Find a MATLAB icon for notifications
    %
    
    if ispref('mnotify', 'icon') && exist(getpref('mnotify', 'icon'), 'file')
        
        icon = getpref('mnotify', 'icon');
        return
        
    else
   
        addpref('mnotify', 'icon', '');
        
        mlRegex = '[Mm][Aa][Tt][Ll][Aa][Bb]*.png';
        mlBinDir = fullfile(matlabroot,'bin');
        mlIcons = mfind(mlBinDir, '-name', mlRegex);
        homeIcons = mfind('~', '-name', mlRegex);
        usrIcons = mfind('/usr/', '-name', mlRegex);
        icons = vertcat(mlIcons, homeIcons, usrIcons);
        
        if isempty(icons)
            icon = '';
        else
            icon = icons(1);
        end
        
        setpref('mnotify', 'icon', icon);
    
    end
    
end