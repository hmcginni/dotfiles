function check(varargin)
	% CHECK - Check for specified licenses in a background task. Attempts to
	% checkout the specified license every 30 seconds.
	%
	%     EXAMPLE:
	%       check Simulink Stateflow Simulink_Test Simulink_Design_Verifier
	
	%% Construct Timer Object
	
	
	if ~iscellstr(varargin)
		errStr = sprintf('\tAll inputs must be product name strings.');
		error(errStr);
	end
	
	for productCode = string(varargin)
		timerName = strcat('checkout_', productCode);
		tObj = timer(...
			'ExecutionMode','fixedSpacing',...
			'Name',timerName,...
			'Period',30,...
			'StartDelay',1);
		tObj.TimerFcn = {@tryLicense, char(productCode)};
		start(tObj);
	end
	
end


%% FUNCTION 'tryLicense' ---------------------------------------------------- %%


function tryLicense(tObj,~,productCode)
	
	[status,msg] = license('checkout', productCode);
	
	if status
		fprintf('%s acquired at [%s]\n', productCode, datestr(now))
		stop(tObj);
		delete(tObj);
	elseif contains(msg, 'Cannot find a license for')
		stop(tObj);
		delete(tObj);
		error('  Unknown product "%s".', productCode);
	end
	
end