package me.excel.tools.validator.sheet;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelSheet;

import java.util.List;

/**
 * excel sheet validator, after workbook validators, if workbook validators failure, sheet validators will skip.
 * <p>
 * Created by hanwen on 2016/12/23.
 */
public interface SheetValidator {

  /**
   * get error message
   *
   * @return
   */
  String getErrorMessage();

  /**
   * get caused by which cell
   *
   * @param excelSheet
   * @return
   */
  List<ExcelCell> getMessageOnCells(ExcelSheet excelSheet);

  /**
   * validate supplied excel sheet
   *
   * @param excelSheet
   * @return
   */
  boolean validate(ExcelSheet excelSheet);
}
