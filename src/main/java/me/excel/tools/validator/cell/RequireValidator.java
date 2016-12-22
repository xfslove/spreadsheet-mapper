package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import org.apache.commons.lang3.StringUtils;

/**
 * required validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class RequireValidator extends CellValidatorAdapter {

  public RequireValidator(String matchField) {
    super(matchField, "应该为必填");
  }

  public RequireValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  public boolean validate(ExcelCell excelCell) throws SkipValidateException {
    return customValidate(excelCell);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) throws SkipValidateException {
    return StringUtils.isNotBlank(excelCell.getValue());
  }
}
