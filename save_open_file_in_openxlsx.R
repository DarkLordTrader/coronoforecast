
save_openxlsx <- function(df_or_list,
                          open_in_xl_only = FALSE,
                          save_file_name,
                          show_decimal = FALSE,
                          open_temp_file = TRUE) {


require(openxlsx)

  ## Create a blank workbook
  wb <- openxlsx::createWorkbook()

  # Number formatting
  options("openxlsx.numFmt" = "0") # No decimal cases formating

  # Formatting for decimal
  if(show_decimal == TRUE){styleT <- createStyle(numFmt = "#,##0.00;[red]-#,##0.00;;") }else{
    styleT <- createStyle(numFmt = "#,##0;[red]-#,##0;;") }


  # Check id list or df and save
  if(class(df_or_list) == "list"){

    # Create individual worksheets
    walk(names(df_or_list),~openxlsx::addWorksheet(wb = wb, sheetName = .x))

    # Write data onto each worksheet
    walk2(1:length(df_or_list), names(df_or_list), ~writeDataTable(wb, .y, df_or_list[[.x]]))

    # Add number formatting to each worksheet
    walk2(names(df_or_list), 1:length(df_or_list),
          ~addStyle(wb = wb, sheet = .x, style = styleT,
                    rows = 2:(nrow(df_or_list[[.y]]) + 1),
                    cols = map_lgl(df_or_list[[.y]], is.numeric) %>% which() %>% as.vector(),
                    gridExpand = TRUE,
                    stack = TRUE)  )
  } else {


    openxlsx::addWorksheet(wb = wb, sheetName = "Report")
    writeDataTable(wb = wb, sheet = "Report", x = df_or_list)

    addStyle(wb = wb,
             sheet = "Report",
             style = styleT,
             rows = 2:(nrow(df_or_list) + 1),
             cols = map_lgl(df_or_list, is.numeric) %>% which() %>% as.vector(),
             gridExpand = TRUE,
             stack = TRUE)

  }



  # Save xl
  if(open_in_xl_only == FALSE){openxlsx::saveWorkbook(wb = wb, file = save_file_name, overwrite = TRUE)}

  # open temp file
  if(open_temp_file == TRUE){openxlsx::openXL(wb)}
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shortcut to open diretly in xl
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xlopen <-  function(df, decimal = FALSE){

  save_openxlsx(df_or_list = df,
                open_in_xl_only = T, show_decimal = decimal)


}

