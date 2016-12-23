package me.excel.tools.validator.row;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * row values validator adapter, easy implements customer value validator extends this.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public abstract class RowValidatorAdapter implements RowValidator {

  private String errorMessage;

  private List<String> matchFields = new ArrayList<>();

  public RowValidatorAdapter(String errorMessage, String[] matchFields) {
    this.errorMessage = errorMessage;
    Collections.addAll(this.matchFields, matchFields);
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public List<ExcelCell> getMessageOnCells(ExcelRow excelRow) {

    List<ExcelCell> causedByCells = new ArrayList<>();
    for (String field : matchFields) {
      causedByCells.add(excelRow.getCell(field));
    }

    return causedByCells;
  }

  @Override
  public boolean validate(ExcelRow excelRow) {
    return customValidate(excelRow);
  }

  protected abstract boolean customValidate(ExcelRow excelRow);
}
