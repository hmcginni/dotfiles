function check(varargin)
	%
	% CHECK - Check for specified licenses in a background task. Attempts to
	%         check out the desired license every 5 seconds.
	%
	%
	%   USAGE:
	%     check Simulink Stateflow Simulink_Test Simulink_Design_Verifier
	%
	
	
	if ~iscellstr(varargin)
		error('All inputs to CHECK must be product name strings.');
	end
	
	for productCode = string(varargin)
		timerName = strcat('checkout_', productCode);
		tObj = timer(...
			'ExecutionMode','fixedSpacing',...
			'Name',timerName,...
			'Period',5,...
			'StartDelay',1);
		tObj.TimerFcn = {@try_license, char(productCode)};
		start(tObj);
	end
	
end


%% Subfunctions


function try_license(tObj,~,productCode)
	%
	% TRY_LICENSE - Try to check out a license for "productCode"
	%
	
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
		error('Unknown product "%s".', productCode);
	end
	
end


function notify_send(title, msg)
	%
	% NOTIFY-SEND - Display message to user
	%
	
	[status,~] = system('which notify-send');
	isNotifySend = ~status;
	
	if isNotifySend
		execStr = sprintf('notify-send "%s" "%s" &', title, msg);
		system(execStr);
	end
	
	fprintf('\n\t*** %s at %s ***\n\n', title, msg);
	
end
