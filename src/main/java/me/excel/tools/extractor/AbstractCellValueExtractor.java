package me.excel.tools.extractor;

/**
 * Created by hanwen on 5/3/16.
 */
public abstract class AbstractCellValueExtractor implements CellValueExtractor {

  private String matchField;

  public AbstractCellValueExtractor(String matchField) {
    this.matchField = matchField;
  }

  protected final String getMatchField() {
    return matchField;
  }

  @Override
  public abstract String getStringValue(Object data);

  @Override
  public boolean matches(String field) {
    return field.equals(matchField);
  }
}
