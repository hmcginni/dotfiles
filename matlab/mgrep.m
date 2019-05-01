function matchList = mgrep(flags, regex, file)
    %
    % MGREP - MATLAB wrapper to `bash` GREP utility
    %
    
    %% Parse Input
    %
    % Allow for two inputs, e.g.
    %   $ grep -rn <pattern>
    %   $ grep <pattern> <file>
    
    if nargin == 2
        
        if ~isempty(regexp(flags,'^-.*r.*','ONCE'))
            file = '';
        else
            file = regex;
            regex = flags;
            flags = '';
        end
        
    end
    
    if ~exist( file, 'file' ) && ~isempty(file)
        error(['File "', file, '" could not be found.']);
    end
    
    %% Verify GREP is on the system PATH
    
    if system('which grep >/dev/null')
        error('GREP utility not found.');
    end
    
    %% Call FIND builtin and parse results
    
    systemCmd = sprintf('grep -E %s "%s" %s 2>/dev/null', flags, regex, file);
    
    [~, matchList] = system(systemCmd);
    
    if isempty(matchList)
        matchList = string.empty(0,1);
        return
    end
    
    matchList(end) = '';
    matchList = strip(string( split(matchList,newline) )); 
    
end