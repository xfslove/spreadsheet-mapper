package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * Created by hanwen on 16/7/7.
 */
public class NumberValidator extends AbstractCellValidator {

  public NumberValidator(String field) {
    super(field, "应该为数字");
  }

  public NumberValidator(String field, String message) {
    super(field, message);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    return NumberUtils.isNumber(excelCell.getValue());
  }
}
