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
   * @return validate error message
   */
  String getErrorMessage();

  /**
   * @param excelRow row
   * @return message on which cells
   */
  List<ExcelCell> getMessageOnCells(ExcelRow excelRow);

  /**
   * validate supplied excel row
   *
   * @param excelRow row
   * @return success
   */
  boolean validate(ExcelRow excelRow);
}
