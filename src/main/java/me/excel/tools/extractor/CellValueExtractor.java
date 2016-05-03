package me.excel.tools.extractor;

/**
 * Created by hanwen on 4/26/16.
 */
public interface CellValueExtractor {

  String getMatchField();

  String getStringValue(Object data, String field);

  boolean matches(String field);
}
