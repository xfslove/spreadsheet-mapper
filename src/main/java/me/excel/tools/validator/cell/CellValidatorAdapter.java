package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.Cell;
import org.apache.commons.lang3.StringUtils;

import java.util.HashSet;
import java.util.Set;

/**
 * <pre>
 * cell value validator adapter, easy implements customer value validator extends this.
 * extends this validator will skip valid when cell value is blank.
 * </pre>
 * Created by hanwen on 15-12-16.
 */
public abstract class CellValidatorAdapter implements CellValidator {

  private int sheetIndex;

  private String key;

  private String matchField;

  private String errorMessage;

  private Set<String> dependsOn = new HashSet<>();

  public CellValidatorAdapter(String matchField, String errorMessage) {
    this.sheetIndex = 1;
    this.key = matchField;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
  }

  public CellValidatorAdapter(String key, String matchField, String errorMessage) {
    this.sheetIndex = 1;
    this.key = key;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
  }

  public CellValidatorAdapter(int sheetIndex, String matchField, String errorMessage) {
    this.sheetIndex = sheetIndex;
    this.key = matchField;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
  }

  public CellValidatorAdapter(int sheetIndex, String key, String matchField, String errorMessage) {
    this.sheetIndex = sheetIndex;
    this.key = key;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
  }

  public CellValidatorAdapter(String matchField, String errorMessage, Set<String> dependsOn) {
    this.sheetIndex = 1;
    this.key = matchField;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.dependsOn = dependsOn;
  }

  public CellValidatorAdapter(String key, String matchField, String errorMessage, Set<String> dependsOn) {
    this.sheetIndex = 1;
    this.key = key;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.dependsOn = dependsOn;
  }

  public CellValidatorAdapter(int sheetIndex, String matchField, String errorMessage, Set<String> dependsOn) {
    this.sheetIndex = sheetIndex;
    this.key = matchField;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.dependsOn = dependsOn;
  }

  public CellValidatorAdapter(int sheetIndex, String key, String matchField, String errorMessage, Set<String> dependsOn) {
    this.sheetIndex = sheetIndex;
    this.key = key;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.dependsOn = dependsOn;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public String getKey() {
    return key;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public Set<String> getDependsOn() {
    return dependsOn;
  }

  @Override
  public boolean validate(Cell cell) {
    return StringUtils.isBlank(cell.getValue()) || customValidate(cell);
  }

  /**
   * for customer access error message
   *
   * @param errorMessage error message
   */
  protected void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  protected abstract boolean customValidate(Cell cell);

}
