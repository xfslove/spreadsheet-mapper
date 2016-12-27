package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * number validator
 * <p>
 * Created by hanwen on 16/7/7.
 */
public class NumberValidator extends CellValidatorAdapter {

  public NumberValidator(String matchField) {
    super(matchField, "应该为数字");
  }

  public NumberValidator(String matchField, String message) {
    super(matchField, message);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    return NumberUtils.isNumber(excelCell.getValue());
  }
}
