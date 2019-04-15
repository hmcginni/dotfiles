function mpushd(new_dir)
    %
    % MPUSHD - MATLAB implementation of bash builtin `pushd`
    %
    
    %% Input Validation
    
    if ~exist(new_dir, 'dir')
        error('Directory "%s" could not be found.', new_dir);
    end
    
    %% Add Current Directory to Stack
    
    current_dir = pwd;
    dir_stack = getenv('DIR_STACK');
    
    if isempty(dir_stack)
        dir_stack = current_dir;
    else
        dir_split = strsplit(dir_stack, ':');
        dir_split{end+1} = current_dir;
        dir_stack = strjoin(dir_split, ':');
    end
    
    setenv('DIR_STACK', dir_stack);
    
    %% Change to new directory
    
    cd(new_dir);