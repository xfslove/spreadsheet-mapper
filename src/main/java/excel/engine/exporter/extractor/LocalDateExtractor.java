package excel.engine.exporter.extractor;

import excel.engine.exception.ExcelProcessException;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.LocalDate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import excel.engine.util.FieldUtils;

/**
 * local date readable value extractor
 *
 * Created by hanwen on 5/3/16.
 */
public class LocalDateExtractor extends FieldValueExtractorAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateExtractor.class);

  private String pattern;

  public LocalDateExtractor(String matchField, String pattern) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public String getStringValue(Object data) {

    try {
      LocalDate value = (LocalDate) PropertyUtils.getProperty(data, FieldUtils.detectRealField(getMatchField()));

      return value == null ? null : value.toString(pattern);

    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelProcessException(e);
    }
  }
}
