package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.ValidateFunction;

/**
 * 根据excel单元格提供的数据, 找到其他数据
 * Created by hanwen on 16/3/18.
 */
public class FindDataValidator extends CommonCellValidator {

  public FindDataValidator(String matchField, ValidateFunction<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, "无法找到对应数据", validateResultGetter);
  }

}
