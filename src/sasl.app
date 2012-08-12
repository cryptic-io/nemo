{application, sasl,
   [{description, "SASL  CXC 138 11"},
    {vsn, "2.1.8"},
    {modules, [sasl, 
	       alarm_handler, 
               format_lib_supp, 
               misc_supp, 
               overload, 
               rb, 
               rb_format_supp, 
	       release_handler, 
	       release_handler_1, 
	       erlsrv,
	       sasl_report, 
	       sasl_report_tty_h, 
	       sasl_report_file_h, 
	       systools, 
	       systools_make, 
	       systools_rc, 
	       systools_relup, 
	       systools_lib
	      ]},
    {registered, [sasl_sup, alarm_handler, overload, release_handler]},
    {applications, [kernel, stdlib]},
    {env, [
        {errlog_type, error},
        {sasl_error_logger, tty}, %Seems to be ignored O_o
        {error_logger_mf_dir, "log"},
        {error_logger_mf_maxfiles,20},
        {error_logger_mf_maxbytes,67108864} %512 MB
        ]},
    {mod, {sasl, []}}]}.

