function fileList = mfind(dir, name_opt, regex)
	%
	% MFIND - MATLAB wrapper to `bash` FIND builtin
	%
	
	%% Parse Input
	
	if ~exist( dir, 'dir' )
		error(['Directory "', dir, '" could not be found.']);
	elseif ~any( contains({'-name','-wholename'}, name_opt) )
		error('Second input to MFIND must be "-name" or "-wholename".');
	end
	
	%% Call FIND builtin and parse results
	
	systemCmd = sprintf('find %s %s "%s" 2>/dev/null', dir, name_opt, regex);
	
	[~, fileList] = system(systemCmd);
	
	if isempty(fileList)
		fileList = string.empty(0,1);
		return
	end
	
	fileList(end) = '';
	fileList = string( split(fileList,newline) );

	
end