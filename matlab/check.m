function check(varargin)
	% CHECK - Check for specified licenses in a background task. Attempts to
	%     checkout the specified license every 30 seconds.
	%
	%     EXAMPLE:
	%         check Simulink Stateflow Simulink_Test Simulink_Design_Verifier
	
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
		tObj.TimerFcn = {@try_license, char(productCode)};
		start(tObj);
	end
	
end


%% FUNCTION 'try_license' --------------------------------------------------- %%


function try_license(tObj,~,productCode)
	
	[status,msg] = license('checkout', productCode);
	
	if status
		title = sprintf('%s license acquired', productCode);
		msg = datestr(now);
		notify_send(title, msg);
		stop(tObj);
		delete(tObj);
	elseif contains(msg, 'Cannot find a license for')
		stop(tObj);
		delete(tObj);
		error('  Unknown product "%s".', productCode);
	end
	
end


%% FUNCTION 'notify_send' --------------------------------------------------- %%


function notify_send(title, msg)

	[notifySendMissing,~] = system('which notify-send');
	
	if ~notifySendMissing
		execStr = sprintf('notify-send "%s" "%s"', title, msg);
		system(execStr);
	end
	
	fprintf('\n\t*** %s at %s ***\n\n', title, msg);
	
end