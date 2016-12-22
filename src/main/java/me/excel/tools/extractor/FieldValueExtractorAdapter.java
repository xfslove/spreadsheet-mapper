package me.excel.tools.extractor;

/**
 * field value extractor adapter, easy implements customer value extractor extends this.
 * <p>
 * Created by hanwen on 5/3/16.
 */
public abstract class FieldValueExtractorAdapter implements FieldValueExtractor {

  private String matchField;

  public FieldValueExtractorAdapter(String matchField) {
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
