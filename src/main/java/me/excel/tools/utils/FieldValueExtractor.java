package me.excel.tools.utils;

/**
 * object field value extractor
 *
 * Created by hanwen on 15-12-16.
 */
public interface FieldValueExtractor {

  String getStringValue(Object data, String field);
}
