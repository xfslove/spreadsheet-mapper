package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * excel cell value validator, after sheet validators, if sheet validators failure, row validators will skip.
 * <p>
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator {

  /**
   * @return validate error message
   */
  String getErrorMessage();

  /**
   * validate supplied excel cell value
   *
   * @param excelCell cell
   * @return success
   */
  boolean validate(ExcelCell excelCell);

  /**
   * matches which field
   *
   * @param field field
   * @return success
   */
  boolean matches(String field);
}
