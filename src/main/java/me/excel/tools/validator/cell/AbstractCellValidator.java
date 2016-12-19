package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import org.apache.commons.lang3.StringUtils;

/**
 * model的field validator父类
 * <p>
 * Created by hanwen on 15-12-16.
 */
public abstract class AbstractCellValidator implements CellValidator {

  protected String matchField;

  /**
   * 错误提示信息
   */
  protected String errorMessage;

  public AbstractCellValidator(String matchField, String errorMessage) {
    this.matchField = matchField;
    this.errorMessage = errorMessage;
  }

  @Override
  public boolean validate(ExcelCell excelCell) throws SkipValidateException {
    return StringUtils.isBlank(excelCell.getValue()) || customValidate(excelCell);
  }

  protected abstract boolean customValidate(ExcelCell excelCell) throws SkipValidateException;

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public boolean matches(ExcelCell cell) {
    return cell.getField().equals(matchField);
  }

  @Override
  public boolean matches(String field) {
    return field.equals(matchField);
  }

}
