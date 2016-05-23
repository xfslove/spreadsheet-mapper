package me.excel.tools.extractor;

import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.LocalDate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static me.excel.tools.FieldUtils.getFieldWithoutPrefix;

/**
 * Created by hanwen on 5/3/16.
 */
public class LocalDateExtractor extends AbstractCellValueExtractor {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateExtractor.class);

  private String pattern;

  public LocalDateExtractor(String matchField, String pattern) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public String getStringValue(Object data, String field) {

    try {
      LocalDate value = (LocalDate) PropertyUtils.getProperty(data, getFieldWithoutPrefix(field));

      return value == null ? null : value.toString(pattern);

    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new IllegalArgumentException(e);
    }
  }
}
