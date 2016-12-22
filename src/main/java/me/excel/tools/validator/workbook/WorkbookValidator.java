package me.excel.tools.validator.workbook;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelWorkbook;

import java.util.List;

/**
 * excel row values validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface WorkbookValidator {

  /**
   * get validate error message
   *
   * @return
   */
  String getErrorMessage();

  /**
   * get caused by which cell
   *
   * @param excelWorkbook
   * @return
   */
  List<ExcelCell> getCausedByCells(ExcelWorkbook excelWorkbook);

  /**
   * validate supplied excel workbook
   *
   * @param excelWorkbook
   * @return
   */
  boolean validate(ExcelWorkbook excelWorkbook);
}
