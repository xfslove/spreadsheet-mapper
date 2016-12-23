package me.excel.tools.validator.row;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;

import java.util.List;

/**
 * excel row values validator, after sheet validators, if sheet validators failure, row validators will skip.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface RowValidator {

  /**
   * get validate error message
   *
   * @return
   */
  String getErrorMessage();

  /**
   * get caused by which cell
   *
   * @return
   */
  List<ExcelCell> getMessageOnCells(ExcelRow excelRow);

  /**
   * validate supplied excel row
   *
   * @param excelRow
   * @return
   */
  boolean validate(ExcelRow excelRow);
}
