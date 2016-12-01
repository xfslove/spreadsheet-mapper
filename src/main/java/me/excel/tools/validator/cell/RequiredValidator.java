package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import org.apache.commons.lang3.StringUtils;

/**
 * Created by hanwen on 15-12-16.
 */
public class RequiredValidator extends AbstractCellValidator {

  public RequiredValidator(String matchField) {
    super(matchField, "应该为必填", "必填");
  }

  public RequiredValidator(String matchField, String errorMessage, String prompt) {
    super(matchField, errorMessage, prompt);
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
