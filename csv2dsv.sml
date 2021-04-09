exception emptyInputFile;
exception WrongFileFormat;
exception UnevenFields;
exception WrongDelimiter;

(* Reading a file and writing in a file and handling if file does not exist *)
fun open_file_in (filename : string) = 
    let
        val temp = TextIO.openIn filename
    in
        temp
    end
fun open_file_out (filename : string) = 
    let
        val temp = TextIO.openOut filename
    in
        temp
    end
fun close_file_in filename = 
    let
       val temp = TextIO.closeIn filename
    in
        ()
    end 
fun reading(filename : string) =
    let 
        val opened_file  = open_file_in(filename)
        val content = TextIO.inputAll opened_file handle e => (close_file_in opened_file; raise e)
        val _ = close_file_in opened_file
    in 
        content 
    end
fun close_file_out filename = 
    let
       val temp = TextIO.closeOut filename
    in
        temp
    end 
fun writing (filename : string, content : string) =
    let 
        val opened_file = open_file_out(filename)
        val _ = TextIO.output (opened_file, content) handle e => (close_file_out opened_file; raise e)
        val _ = close_file_out opened_file
    in 
        () 
    end


(* Checking if the input file is in the correct format *)
fun checking(str : string , delim1) = 
    let
        val sz = size str; 
        val temp = 0;

        (* If the input file is empty *)
        val temp = if sz > 0 then 0 else ~1;
        val temporary = if temp = ~1 then (print("The File is Empty"); raise emptyInputFile) else 0;

        (* IF the last row doesn't end with a new line character *)
        val temp = if temp > ~1 andalso String.sub(str,sz-1) = #"\n" then 0 else ~1
        val temporary = if temp = ~1 then (print("No newline character at the end of file"); raise WrongFileFormat) else 0



        (* Checking if all the rows has same number of fields *)
        (* First I make a list of index where a row ends
        After that I recursively call for each row and keep checking if the number of fields is equal to the number of fields 
        in 1st row if not then return the [-1,rownumber,number of fields] and raise an exception *)
        fun making_list_of_endline_indexes(x : string, index : int , sz : int ,cnt : int, list_of_index : int list) = 
        let
            val dump1 = if String.sub(x,index) = #"\n" andalso index < sz - 1  andalso  cnt mod 2 = 0 then 1 else 0
            val to_append = [index];
            val list_of_index = if dump1 = 1 then list_of_index @ to_append else list_of_index

            val dump1 = if String.sub(x,index)  = #"\"" then 1 else 0
            val cnt = cnt + dump1;

        in
            if index = sz -1 then list_of_index else making_list_of_endline_indexes(x,index+1,sz,cnt,list_of_index)
        end
        
        fun no_of_columns(x : string, index : int, end_of_row : int,cnt : int,cols:int) = 
        let
            val dump1  = if cnt mod 2 = 0 andalso String.sub(x,index) = delim1 then 1 else 0;
            val cols = cols + dump1;
            val dump1 = if String.sub(x,index) = #"\"" then 1 else 0;
            val cnt = cnt + dump1;
        in
            if index = end_of_row then cols else no_of_columns(x,index+1,end_of_row,cnt,cols)
        end
        
        fun checking_cols(x:string,index : int, cols:int,l : int list) = 
        let
            val dump = 0;
            val row_initial = List.nth(l,index)+1;
            val row_final = List.nth(l,index+1);
            val dump1 = no_of_columns(x,row_initial,row_final,0,dump);
            val check = if dump1 = cols then 1 else ~1
            val sz = length l;
        in 
            if check < 0 then [~1,index,dump1] else
            (
                if index < sz - 2 then checking_cols(x,index+1,cols,l) else [0,0,0]
            )
        end
        val sz = size str;
        val list_of_index_final = [sz-1];
        val dump_list_dump = [];
        val temp_list = making_list_of_endline_indexes(str,0,sz,0,dump_list_dump);
        val list_of_index_final = temp_list @ list_of_index_final;
        val variable_dumped = 0;
        val first_row_columns = no_of_columns(str,0,List.nth(list_of_index_final,0),0,variable_dumped);
        val checking_columns = checking_cols(str,0,first_row_columns,list_of_index_final);
        val temporary = if List.nth(checking_columns,0)= ~1 then
        (
            print("Expected: ");
            print(Int.toString(first_row_columns+1));
            print(" fields , Present: ");
            print(Int.toString(List.nth(checking_columns,2)+1));
            print(" fields on Line ");
            print(Int.toString(List.nth(checking_columns,1)+2));
            raise UnevenFields
        ) 
        else 0



        (* Checking if double quotes are in desired format *)
        (* If the current double quote is the odd numbered double quote then one of the following should be satisfied
            Present at index 0
            There is a delim1 or newline character preceeding it
            There is a double quote preceeding it
        If the current double quote is the even numbered double quote then one of the following should be satisfied
            There is a delim1 or newline character following it
            There is a double quote following it *)
        fun double_quotes_checking(x : string , index : int , cnt : int, sz : int, value_to_return  : int) =
        let
            val dump1 = if String.sub(x,index) = #"\"" then
            (
                let
                    val dump2 = if cnt mod 2 = 1 then
                    (
                        let
                            val dump3 = if String.sub(x,index+1)= delim1 orelse String.sub(x,index+1) = #"\"" orelse String.sub(x,index+1) = #"\n" then 0 else ~1
                        in
                            dump3
                        end
                    )
                    else
                    (
                        let
                            val dump3 = if index = 0 orelse String.sub(x,index-1)= delim1 orelse String.sub(x,index-1) = #"\"" orelse String.sub(x,index-1) = #"\n" then 0 else ~1
                        in
                            dump3
                        end
                    )
                in
                    dump2
                end
            )
            else
            (
                0
            )
            val value_to_return = dump1;
            val dump1 =  if String.sub(x,index) = #"\"" then 1 else 0
            val cnt = cnt + dump1;
        in
            if value_to_return < 0 orelse index = sz-1 then value_to_return else double_quotes_checking(x,index+1,cnt,sz,value_to_return)
        end 
        val sz = size str;
        val temp = double_quotes_checking(str,0,0,sz,0);
        val temporary = if temp = ~1 then (print("Double Quotes are not present in the desired format"); raise WrongFileFormat) else 0;


    in 
        temp
    end 

(* Converting the delimiter of the input file *)
fun convertDelimiters(infilename, delim1, outfilename, delim2) =  
    let
        
        val checking_delim = if delim1 = #"\n" orelse delim1 = #"\"" orelse delim2 = #"\n" orelse delim2 = #"\"" then ~1 else 0;
        (* Checking if Delim1 and delim2 are not #"\n" or #"\"" *)
        val temporary = if checking_delim = ~1 then (print("Wrong Delimiters"); raise WrongDelimiter) else 0;
        val content = reading(infilename)
        val content  = content ^ ""
        (* Validating the input file *)
        val dumped = checking(content,delim1);



        (* Converting the content of input file according to delim1 and delim2 *)
        (* index represents the current position where i am in the string content.
        cnt represents the number of double quotes encountered till now.
        endofline is 1 if a double quote is to be inserted at this position in output file else 0
        sz is the size of the string content
        temp is the string that has to be written in the output file.
        I am doing Tail recursion 
        In the output file Every Field would be inside a double quote and hence any delim2 would be escaped when reading the output file *)
        fun solve(x : string, index : int,cnt : int, endofline : int,sz : int,temp :string) = 
            let
                val dump1 = if String.sub(x,index) = #"\"" then 1 else 0;
                val cnt = cnt + dump1;
                val endofline = if dump1 = 1 then 1 else endofline;
                
                val temp_bool_variable = String.sub(x,index) = delim1 orelse String.sub(x,index) = #"\n"
                val dump1 = if index = 0 andalso temp_bool_variable then "\"" else ""
                val temp = temp ^ dump1;


                val dump1 = if endofline = 1 then "\"" else "";
                val temp = temp ^ dump1;
                val endofline = 0;

                val dump_temp = if index > 0 andalso String.sub(x,index-1) = #"\"" then 1 else 0;
                val temp_boolean = if String.sub(x,index) = delim1 orelse String.sub(x,index) = #"\n" then 1 else 0
                val dump1 = if dump_temp = 0 andalso temp_boolean = 1 then
                (
                    if cnt mod 2 = 1 then 0 else 1
                )
                else 0;
                val temp = if dump1 = 1 then temp ^ "\"" else temp; 
                val dump1 = if String.sub(x,index) = delim1 then
                (
                    let
                        val dump2 = if cnt mod 2 = 1 then delim1 else delim2
                    in
                        String.str(dump2)
                    end
                )
                else
                (
                    
                    if String.sub(x,index) = #"\""  then "" else String.str(String.sub(x,index))
                )
                
                val temp = temp ^ dump1;

                
                val dump1 = if String.sub(x,index) = delim1 orelse String.sub(x,index) = #"\n" then
                (
                    let
                        val dump2 = if cnt mod 2 =1 then 0 else 1
                    in
                        dump2
                    end
                ) 
                else
                (
                    0
                )
                val endofline = dump1;
                
                

               
            in
                if index+1=sz then temp else solve(x,index+1,cnt,endofline,sz,temp)
            end
        val sz = size content;
        val output_content = "";
        val temp1 = solve(content,0,0,1,sz,output_content);
        val tt = writing(outfilename,temp1);

    in
        temp1
    end
    
fun csv2tsv(infilename, outfilename) = 
    let
        val dump = convertDelimiters(infilename,#",",outfilename,#"\t");
    in
        1
    end
fun tsv2csv(infilename, outfilename) = 
    let
        val dump = convertDelimiters(infilename,#"\t",outfilename,#",");
    in
        1
    end




fun convertNewlines(infilename, newline1, outfilename, newline2)=
    let
        val content = reading(infilename);
        val content  = content ^ "";
        fun solve(x : string , index : int , sz : int , temp : string) = 
        let
            val increment = 1;
            val size_newline1 = size newline1;
            val dump = if sz - index + 1 > size_newline1 then
            (
                let
                    val dump1 = if String.compare(substring(x,index,size_newline1),newline1)=EQUAL then 1 else 0;
                in
                    dump1
                end
            )    
            else
            (
                0
            )
            val increment = if dump = 1 then size_newline1 else increment;
            val temp = if dump =1 then temp ^ newline2 else temp ^ String.str(String.sub(x,index));

        in
            if index = sz -1 then temp else solve(x,index+increment,sz,temp)
        end
        val sz = size content;
        val output_content = "";
        val temp1 = solve(content,0,sz,output_content);
        val tt = writing(outfilename,temp1);
    in
        temp1
    end 