function mnotify(title, msg)
    %
    % MNOTIFY - Use `notify-send` to display message to user
    %
    
    if nargin == 1
        msg = title;
        title = 'MATLAB';
    end
    
    [status,~] = system('which notify-send');
    existNotifySend = ~status;
    matlab_icon_path = get_matlab_icon_path();
    
    if ~isempty(matlab_icon_path)
        png_flags = sprintf('-i %s', matlab_icon_path);
    else
        png_flags = '';
    end
    
    numWords = numel(regexp(msg,'\w*[ .]'));
    timeout = num2str(200*numWords + 4000);
    opts = ['-t ', timeout, ' ', png_flags];
    
    if existNotifySend
        execStr = sprintf('notify-send %s "%s" "%s" &', opts, title, msg);
        system(execStr);
    else
        fprintf('\n\t*** %s %s ***\n\n', title, msg);
    end
    
end



function icon = get_matlab_icon_path()
    
    icon = fullfile(matlabroot,'toolbox/nnet/nnresource/icons/matlab.png');
    if ~exist(icon, 'file')
        icons = mfind('~','-name','*[Mm][Aa][Tt][Ll][Aa][Bb]*.png');
        if isempty(icons)
            icon = '';
        else
            icon = icons(1);
        end
    end
    
end