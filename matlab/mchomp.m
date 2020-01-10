function strOut = mchomp(strIn)
	
	if isempty(strIn)
		strOut = strIn;
		return
	end
	
	switch class(strIn)
		
      case "char"
        if isequal(strIn(end), newline)
            strIn(end) = '';
        end
        
      case "string"
        if isequal(numel(strIn), 1)
            if isequal(strfind(strIn,newline), strlength(strIn))
                strIn(end) = "";
            end
        else
            if isequal(strIn(end),"")
                strIn(end) = [];
            end
        end
        
      case "cell"
        if iscellstr(strIn) && isempty(deblank(strIn{end}))
            strIn(end) = '';
        end
	end
	
	strOut = strIn;
    
end
