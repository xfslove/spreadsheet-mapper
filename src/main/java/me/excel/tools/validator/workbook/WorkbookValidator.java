package me.excel.tools.validator.workbook;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelWorkbook;

/**
 * Created by hanwen on 4/26/16.
 */
public interface WorkbookValidator {

  String getErrorMessage();

  ExcelCell getMessageOnCell(ExcelWorkbook excelWorkbook);

  boolean validate(ExcelWorkbook excelWorkbook);
}
