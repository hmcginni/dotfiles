function out = sc(cmd, alias, dir)
% SC
%
%    Create directory shortcuts
%    Stored in <userpath>/shortcuts.txt
%
%    Usage:
%        sc [command] [alias] [dir]
%        sc('save|remove|list','alias','dir');
%        sc [alias]
%
%    Commands:
%        save   (add)                 -> add alias to cache
%        remove (rm|rem|d|del|delete) -> remove alias from cache
%        list   (show|ls)             -> show cache
%        clip   (copy)                -> copy alias to clipboard
%        <none> [alias]               -> go to dir specified by alias
	
	
%% PARSE INPUTS  ----------------------------------------------------%%
	
	
	switch nargin
      case 0
        cmd = 'list';
      case 2
        dir = pwd;
	end
	
	
	%% LOAD SHORTCUTS  --------------------------------------------------%%
	
	
    home = getenv('HOME');
	shortcutCache = fullfile(home, 'sc.txt');

	if exist(shortcutCache,'file')
		shortcuts = readtable(shortcutCache, 'Delimiter', ',');
		if isempty(shortcuts)
			shortcuts = table( {}, {}, ...
                               'VariableNames',{'Shortcuts','Directories'});
		end
	else
		shortcuts = table({},{},'VariableNames',{'Shortcuts','Directories'});
	end
	
	
	%% MAIN  ------------------------------------------------------------%%
	
	
	switch cmd
      case {'add','save'}
        % 'save' new alias by adding it to shortcut map -------------%%
        
        alias = lower(alias);
        
        if ~any( cellfun( @(x)strcmpi(x,alias), shortcuts.Shortcuts ) )
            newShortcut = table( {alias}, {dir}, ...
                                 'VariableNames',{'Shortcuts','Directories'});
            shortcuts = sortrows([shortcuts; newShortcut],1);
        else
            shortcuts( strcmp(shortcuts.Shortcuts, alias), : ) = ...
                {alias, dir};
        end
        
        writetable(shortcuts, shortcutCache);
        
      case {'rm','rem','remove','d','del','delete'}
        % 'remove' specified alias ----------------------------------%%
        
        idxMask = cellfun( @(x)strcmp(x,alias), shortcuts.Shortcuts );
        shortcuts(idxMask,:) = [];
        writetable(shortcuts, shortcutCache);
        
      case {'ls','list','show'}
        % 'list' all saved aliases ----------------------------------%%
        
        newDirs = strrep(shortcuts.Directories, userpath, '<userpath>');
        newDirs = strrep(newDirs, getenv('HOME'), '~');
        disp( table( shortcuts.Shortcuts, newDirs, ...
                     'VariableNames',{'Shortcuts','Directories'} ) );
        
      case {'clip','copy'}
        % 'copy' specified alias path to clipboard ------------------%%
        
        idxMask = cellfun( @(x)strcmpi(x,alias), shortcuts.Shortcuts );
        thisDir = char( shortcuts{idxMask,2} );
        clipboard( 'copy', thisDir );
        
      case {'getaliases'}
        % 'getaliases' output aliases only --------------------------%%
        
        out = shortcuts.Shortcuts;
        
      case {'gettable'}
        % 'gettable' output table only ------------------------------%%
        
        out = shortcuts;
        
      otherwise
        % cd to specified shortcut, or generate error ---------------%%
        
        if strcmp(cmd, '-')
            mpopd;
        end
        
        isCmd = cellfun( @(x)strcmpi(x,cmd), shortcuts.Shortcuts );
        
        if any( isCmd )
            sc('save','-',pwd);
            newDir = shortcuts{ isCmd, 2};
            mpushd(char(newDir));
        else
            error('Unrecognized command "%s".',cmd)
        end
        
	end


	%% END --------------------------------------------------------------%%
