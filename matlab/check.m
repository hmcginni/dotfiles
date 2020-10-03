function check(varargin)
	%
	% CHECK - Check for specified licenses in a background task. Attempts to
	%         check out the desired license every 5 seconds.
	%
	%
	%   USAGE:
	%     check Simulink Stateflow Simulink_Test Simulink_Design_Verifier
	%
	
	
    if isequal(nargin, 0)
        error('No product code specified.');
        
    end

	for feature = string(varargin)
        if license('test', feature) && isempty(license('inuse', feature))
            timerName = strcat('checkout_', feature);
            tObj = timer(...
                'ExecutionMode', 'fixedSpacing', ...
                'Name', timerName, ...
                'Period', 30, ...
                'StartDelay', 1 ...
            );
            tObj.TimerFcn = {@check_license, char(feature)};
            start(tObj);

        elseif ~license('test', feature)
            warning('Unknown product "%s".', feature);

        else
            warning('Product "%s" is already in use.', feature);

        end

	end
	
end


%% Subfunctions


function check_license(tObj, ~, feature)
	%
	% CHECK_LICENSE - check availability of a "feature" license
	%
	
    available = is_available(tObj, feature);
	
	if available
        stop(tObj);
        delete(tObj);
        [status, msg] = license('checkout', feature);

        if status
            title = sprintf('%s license acquired', feature);
            msg = datestr(now);
            mnotify(title, msg);
            
        end
        
    end
	
end


function available = is_available(tObj, feature)
    
    lmutil = fullfile(matlabroot, 'etc', 'glnxa64', 'lmutil');
    licFile = fullfile(matlabroot, 'licenses', 'network.lic');
    lmstat = [lmutil, ' lmstat -f ', feature, ' -c ', licFile];

    [~, stdout] = system(lmstat);
    lines = splitlines(stdout);
    isUsersLine = contains(lines, 'Users');

    if ~any(isUsersLine)
        stop(tObj);
        delete(tObj);
        error('Unable to reach license server.');

    end

    
    usersLine = lines{isUsersLine};
    numIssuedCell = regexp(usersLine, '(\d+) \w* issued', 'tokens');
    numInUseCell = regexp(usersLine, '(\d+) \w* in use', 'tokens');
    numIssued = str2num(numIssuedCell{1}{1});
    numInUse = str2num(numInUseCell{1}{1});

    available = numInUse < numIssued;

end