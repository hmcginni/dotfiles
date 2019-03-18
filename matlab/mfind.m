function fileList = mfind(dir, name_opt, name_regex)
	%
	% MFIND - MATLAB wrapper to `bash` FIND builtin
	%
	
	%% Parse Input
	
	if ~exist( dir, 'dir' )
		error(['Directory "', dir, '" could not be found.']);
	elseif ~any( contains({'-name','-wholename'}, name_opt) )
		error('Second input must be "-name" or "-wholename".');
	end
	
	%% Call FIND builtin and parse results
	
	systemCmd = sprintf('find %s %s "%s"', dir, name_opt, name_regex);
	
	[~, fileList] = system(systemCmd);
	
	fileList = split( fileList, newline );
	remove = contains(fileList, 'find:') | strlength(fileList) == 0;
	fileList(remove) = [];
	
end