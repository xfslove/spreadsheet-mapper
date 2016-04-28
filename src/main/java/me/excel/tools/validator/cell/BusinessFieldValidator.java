package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import me.excel.tools.validator.ValidateFunction;
import org.apache.commons.lang3.StringUtils;

/**
 * 包含必填的验证
 * Created by hanwen on 4/26/16.
 */
public class BusinessFieldValidator extends CommonCellValidator {

  public BusinessFieldValidator(String matchField, ValidateFunction<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, "无法找到对应数据", "业务主键", validateResultGetter);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) throws SkipValidateException {
    if (StringUtils.isBlank(excelCell.getValue())) {
      return false;
    }
    return super.customValidate(excelCell);
  }
}
