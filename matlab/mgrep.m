function matchList = mgrep(varargin)
    %
    % MGREP - MATLAB wrapper to `bash` GREP utility
    %
    
    %% Verify GREP is on the system PATH
    
    if system('which grep >/dev/null')
        error('GREP utility not found.');
    end
    
    %% Call FIND builtin and parse results

    grepArgs = repmat('%s ', 1, nargin);
    systemCmdFmt = sprintf('grep -E %s 2>/dev/null', grepArgs);
    systemCmd = sprintf(systemCmdFmt, varargin{:});
    
    [status, matchList] = system(systemCmd);
    
    if status
        matchList = string.empty(0,1);
        return
    end
    
    matchList(end) = '';
    matchList = strip(string(split(matchList,newline))); 
    
end