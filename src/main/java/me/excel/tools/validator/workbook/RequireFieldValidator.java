package me.excel.tools.validator.workbook;

import me.excel.tools.ExcelConstants;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelWorkbook;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * required field validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class RequireFieldValidator implements WorkbookValidator {

  private List<String> requireFields = new ArrayList<>();

  private ExcelWorkbook excelWorkbook;

  public RequireFieldValidator(List<String> requireFields) {
    this.requireFields = requireFields;
  }

  @Override
  public String getErrorMessage() {
    if (this.excelWorkbook == null) {
      return ExcelConstants.EMPTY_VALUE;
    }
    return "不包含所要求的字段:" + StringUtils.join(getLostFields(this.excelWorkbook), ExcelConstants.SEPARATOR);
  }

  @Override
  public List<ExcelCell> getCausedByCells(ExcelWorkbook excelWorkbook) {
    return Collections.singletonList(excelWorkbook.getFirstSheet().getRow(1).getCell(1));
  }

  @Override
  public boolean validate(ExcelWorkbook excelWorkbook) {
    boolean result = getLostFields(excelWorkbook).isEmpty();
    if (!result) {
      this.excelWorkbook = excelWorkbook;
      return false;
    }
    return true;
  }

  private List<String> getLostFields(ExcelWorkbook excelWorkbook) {

    List<String> keyRowFields = excelWorkbook.getFirstSheet().getKeyRowFields();
    return requireFields.stream()
        .filter(requiredField -> !keyRowFields.contains(requiredField))
        .collect(Collectors.toList());
  }
}
