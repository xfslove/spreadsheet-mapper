package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.lang3.StringUtils;

/**
 * Created by hanwen on 15-12-16.
 */
public class RequiredValidator extends AbstractFieldValidator {

  public RequiredValidator(String field) {
    super(field, "数据不正确, 应该为必填", "必填");
  }

  public RequiredValidator(String field, String message, String prompt) {
    super(field, message, prompt);
  }

  @Override
  public boolean validate(ExcelCell excelCell) {
    return StringUtils.isNotBlank(excelCell.getValue());
  }
}
