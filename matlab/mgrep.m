function matchList = mgrep(varargin)
    %
    % MGREP - MATLAB wrapper to `bash` GREP utility
    %
    
    %% Verify either grep or ripgrep is on the system PATH

    if ~system('which rg >/dev/null')
        grepCmd = 'rg --no-heading';
    elseif ~system('which grep >/dev/null')
        grepCmd = 'grep -E';
    else
        error('GREP utility not found.');
    end
    
    %% Call FIND builtin and parse results

    grepArgs = repmat('%s ', 1, nargin);
    systemCmdFmt = sprintf('%s %s 2>/dev/null', grepCmd, grepArgs);
    systemCmd = sprintf(systemCmdFmt, varargin{:});
    
    [status, matchList] = system(systemCmd);
    
    if status
        matchList = string.empty(0,1);
        return
    end
    
    matchList(end) = '';
    matchList = strip(string(split(matchList,newline))); 
    
end