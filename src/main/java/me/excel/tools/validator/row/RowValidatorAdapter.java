package me.excel.tools.validator.row;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.validator.SkipValidateException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * row values validator adapter, easy implements customer value validator extends this.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public abstract class RowValidatorAdapter implements RowValidator {

  private String errorMessage;

  private List<String> causedByFields = new ArrayList<>();

  public RowValidatorAdapter(String errorMessage, String[] causedByFields) {
    this.errorMessage = errorMessage;
    Collections.addAll(this.causedByFields, causedByFields);
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public List<ExcelCell> getCausedByCells(ExcelRow excelRow) {
    return causedByFields.stream().map(excelRow::getCell).collect(Collectors.toList());
  }

  @Override
  public boolean validate(ExcelRow excelRow) throws SkipValidateException {
    return customValidate(excelRow);
  }

  protected abstract boolean customValidate(ExcelRow excelRow) throws SkipValidateException;
}
