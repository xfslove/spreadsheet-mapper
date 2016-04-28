package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.ValidateFunction;

/**
 * 唯一的验证
 * Created by hanwen on 4/27/16.
 */
public class UniqueValidator extends CommonCellValidator {

  public UniqueValidator(String matchField, ValidateFunction<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, "数据已存在", "唯一", validateResultGetter);
  }

  public UniqueValidator(String matchField, String errorMessage, ValidateFunction<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, errorMessage, "唯一", validateResultGetter);
  }
}
