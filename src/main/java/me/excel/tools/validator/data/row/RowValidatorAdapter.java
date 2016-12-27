package me.excel.tools.validator.data.row;


import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.message.DataValidateMessage;

import java.util.HashSet;
import java.util.Set;

/**
 * row values validator adapter, easy implements customer value validator extends this.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public abstract class RowValidatorAdapter implements RowValidator {

  private String key;

  private String errorMessage;

  private Set<String> matchFields = new HashSet<>();

  public RowValidatorAdapter(String key, String errorMessage, Set<String> matchFields) {
    this.key = key;
    this.errorMessage = errorMessage;
    this.matchFields = matchFields;
  }

  @Override
  public String getKey() {
    return key;
  }

  @Override
  public DataValidateMessage validate(ExcelRow excelRow) {
    return customValidate(excelRow);
  }

  protected abstract DataValidateMessage customValidate(ExcelRow excelRow);
}
