package me.excel.tools.validator.row;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.validator.SkipValidateException;

import java.util.List;

/**
 * excel row values validator
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
  List<ExcelCell> getCausedByCells(ExcelRow excelRow);

  /**
   * validate supplied excel row
   *
   * @param excelRow
   * @return
   * @throws SkipValidateException
   */
  boolean validate(ExcelRow excelRow) throws SkipValidateException;
}
