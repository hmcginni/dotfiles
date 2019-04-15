function mpopd
    %
    % MPOPD - Pop the directory stack and go to the popped directory
    %
    
    %% Pop last element of DIR_STACK
    
    dir_stack = getenv('DIR_STACK');
    
    if isempty(dir_stack)
        error('No DIR_STACK available.');
    end
    
    dir_split = strsplit(dir_stack, ':');
    dest_dir = dir_split{end};
    dir_split(end) = '';
    
    %% Change to specified directory
    
    cd(dest_dir);
    
    %% Update DIR_STACK environment variable
    
    dir_stack = strjoin(dir_split, ':');
    setenv('DIR_STACK', dir_stack);