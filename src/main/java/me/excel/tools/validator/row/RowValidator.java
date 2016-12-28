package me.excel.tools.validator.row;


import me.excel.tools.model.excel.Row;
import me.excel.tools.validator.DataValidator;

/**
 * excel row values validator, after sheet validators.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface RowValidator extends DataValidator {

  /**
   * valid supplied excel row
   *
   * @param row row
   * @return result
   */
  boolean validate(Row row);

  /**
   * @return which sheet this validator belong to
   */
  int getSheetIndex();
}
