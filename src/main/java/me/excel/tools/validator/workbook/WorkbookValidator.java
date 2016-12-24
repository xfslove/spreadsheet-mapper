package me.excel.tools.validator.workbook;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelWorkbook;

import java.util.List;

/**
 * excel workbook validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface WorkbookValidator {

  /**
   * @return validate error message
   */
  String getErrorMessage();

  /**
   * @param excelWorkbook workbook
   * @return message on which cells
   */
  List<ExcelCell> getMessageOnCells(ExcelWorkbook excelWorkbook);

  /**
   * validate supplied excel workbook
   *
   * @param excelWorkbook workbook
   * @return success
   */
  boolean validate(ExcelWorkbook excelWorkbook);
}
